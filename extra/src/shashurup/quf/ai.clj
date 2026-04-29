(ns shashurup.quf.ai
  (:require [clojure.string :as s]
            [clj-http.client :as http]
            [shashurup.quf.secrets :as secrets]
            [shashurup.quf.response :as r]
            [shashurup.quf.ui :as ui]
            [cheshire.core :as json])
  (:import (java.io BufferedReader InputStreamReader OutputStreamWriter)
           (java.lang ProcessBuilder)
           (java.util.concurrent TimeUnit)))

(def ^:dynamic *default-endpoint* "/api/v1/chat/completions")
(def ^:dynamic *default-message* "You are a large language model and a helpful assistant. Respond concisely.")

(def storage (atom {}))

(defn store-value
  "Stores an arbitrary value into the Quasi Una Fantasia (QuF) environment."
  {:mcp-params
   {:properties {:name {:type "string"
                        :description "The name of the value to store"}
                 :value {:type "any"
                         :description "The value to store"}}}}
  [name value]
  (swap! storage assoc name value)
  nil)

(defn load-value
  "Loads an arbitrary value from the Quasi Una Fantasia (QuF) environment."
  {:mcp-params
   {:properties {:name {:type "string"
                        :description "The name of the value to load"}}}}
  [name]
  (get @storage name))

(defmulti init-server (fn [subj]
                        (cond
                          (:cmd subj) :stdio
                          (:url subj) :http
                          (:fn subj) :fn)))

(defmulti call-server (fn [s & _] (:type s)))

(defmulti close-server :type)

(defmethod init-server :stdio [{:keys [name cmd]}]
  (let [p (.start (ProcessBuilder. cmd))]
    {:type :stdio
     :name name
     :in (BufferedReader. (InputStreamReader. (.getInputStream p)))
     :err (BufferedReader. (InputStreamReader. (.getErrorStream p)))
     :out (OutputStreamWriter. (.getOutputStream p))
     :process p}))

(def mcp-msg-id (atom 0))

(defmethod call-server :stdio [{:keys [in out]} method params]
  (if (= method "notifications/initialized")
      (.write out (str (json/generate-string {:jsonrpc "2.0"
                                              :method method}) "\n"))
      (let [msg {:jsonrpc "2.0"
                 :method method
                 :params params
                 :id (swap! mcp-msg-id inc)}]
        (.write out (str (json/generate-string msg) "\n"))
        (.flush out)
        (json/parse-string (.readLine in) true))))

(defmethod close-server :stdio [{:keys [out process]}]
  (.close out)
  (try
    (when-not (.waitFor process 5 TimeUnit/SECONDS)
      (.destroyForcibly process))
    (catch InterruptedException e
      (.destroyForcibly process))))

(defmethod init-server :fn [subj]
  (let [{srv-name :name fn :fn} subj
        {:keys [arglists doc mcp-params]} (meta fn)
        by-count (group-by count arglists)
        [_ [required]] (apply min-key first by-count)
        [_ [longest]] (apply max-key first by-count)
        all (->> longest
                 (map #(name %))
                 (remove #(= % "&")))
        deduced {:$schema "http://json-schema.org/draft-07/schema#"
                 :type "object"
                 :properties (into {}
                                   (for [p all]
                                     [p {:type "string"}]))
                 :required (mapv name required)}]
    {:type :fn
     :fn (var-get fn)
     :arg-list all
     :mcp-description {:name (name (symbol fn))
                       :description doc
                       :inputSchema (merge deduced mcp-params)}
     :name srv-name}))

(defmethod call-server :fn [{:keys [fn arg-list mcp-description]}
                            method
                            {:keys [_ arguments]}]
  (condp = method
    "tools/list" {:result {:tools [mcp-description]}}
    "tools/call" (let [resp (apply fn (map #(get arguments %) arg-list))]
                   {:result {:content [{:type "text"
                                        :text (json/generate-string resp)}]}})
    nil))

(defmethod close-server :fn [_])

(defn- call-tool [context fun args]
  (let [[server-name name] (get-in context [:dispatch fun])
        srv (get-in context [:servers server-name])]
    (call-server srv "tools/call" {:name name
                                   :arguments (json/parse-string args)})))

(defn- extract-tool-text [subj]
  (s/join " "
          (->> (get-in subj [:result :content])
               (filter #(= (:type %) "text"))
               (map :text))))

(defn- query-model [host endpoint key model messages tools]
  (:body (http/post (str host endpoint)
                    {:headers {"Authorization" (str "Bearer " key)}
                     :content-type :json
                     :as :json
                     ;; :debug true
                     ;; :debug-body true
                     :form-params {:model model
                                   :messages messages
                                   :tools tools}})))

(defn- call-tools [context topic subj tools-callback]
  (update-in context
             [:messages topic]
             into (for [{{fun :name
                          args :arguments} :function
                         id :id} subj]
                    (do
                      (when tools-callback
                        (tools-callback fun))
                      (let [r (call-tool context fun args)]
                        {:role "tool"
                         :tool_call_id id
                         :content (extract-tool-text r)})))))

(defn- ensure-system-message [context topic]
  (let [messages (get-in context [:messages topic])
        {{config-message :message} :config} context]
    (if messages
      context
      (assoc-in context
                [:messages topic] [{:role "system"
                                    :content (or config-message
                                                 *default-message*)}]))))

(defn interact
  "Perform model interaction cycle.
   With tools call if necessary etc."
  [context topic tools-callback]
  (let [{{host :host
          key :key
          endpoint :endpoint
          model :model} :config
         tools :tools} context
        key (if (map? key) (secrets/lookup key) key)]
    (let [resp (query-model host
                            (or endpoint *default-endpoint*)
                            key
                            model
                            (get-in context [:messages topic])
                            tools)
          message (-> resp :choices first :message)
          context (update-in context [:messages topic] conj message)]
      (if-let [tool_calls (:tool_calls message)]
        (let [context (call-tools context topic tool_calls tools-callback)]
          (interact context topic tools-callback))
        context))))

(defn agent [config]
  (atom {:config config}))

(def ^:dynamic *current*)

(defn c
  "Set current model connection. Stored in *current*.
   An argument can be either:
   map - connection configuration consisting of:
     :host - model API host
     :endpoint - optional API endpoint when differ from default
     :key - API key (or a map to retrieve it from secrets)
     :message - system message
     :model - model name to use
     :servers - MCP servers, vector of maps with:
       :name - server name
       :cmd - vector with command and args to start it
       :url - remote server (not yet implemented)"
  [subj]
  (if (map? subj)
    (def ^:dynamic *current* (atom {:config subj}))
    (def ^:dynamic *current* subj)))

(defn- delete-messages [context topic msg-nums]
  (swap! context
         update-in [:messages topic]
         (fn [ms]
           (->> ms
                (keep-indexed (fn [idx m] (if (msg-nums idx) nil m)))
                vec))))

(defn clear
  "Clear model context. Args can be:
   context - optional, atom keeping model interaction state
             when omitted, *current* is used.
   topic - optional, a keyword representing a topic to clear"
  ([] (clear *current*))
  ([arg] (if (or (coll? arg) (keyword? arg))
           (clear *current* arg)
           (swap! arg dissoc :messages)))
  ([context arg]
   (if (keyword? arg)
     (swap! context
            update :messages
            dissoc arg)
     (if (keyword? (first arg))
       (let [[topic num] arg
             nums (set (if (coll? num) num [num]))]
         (delete-messages context topic nums))
       (doseq [[topic pairs] (group-by first arg)]
         (let [nums (set (map second pairs))]
           (delete-messages context topic nums)))))
   arg))

(defn p
  "Prompt the model. Args are:
   query - the prompt
   context - optional, atom keeping model interaction state
             when omitted, *current* is used.
   topic - optional, a keyword representing a discussion topic
           topic holds separate context"
  ([query] (p *current* :default query))
  ([arg query] (if (keyword? arg)
                 (p *current* arg query)
                 (p arg :default query)))
  ([context topic query]
   (r/report-progress (str "Querying " (get-in @context [:config :model])))
   (let [tools-callback (fn [fun]
                          (r/report-progress (str "Using tool " fun)))
         new-context (-> @context
                         (ensure-system-message topic)
                         (update-in [:messages topic]
                                    conj {:role "user"
                                          :content query})
                         (interact topic tools-callback))]
     (reset! context new-context)
     (-> new-context
         (get-in [:messages topic])
         last
         :content
         vector
         (r/hint :markdown)))))

(def btw-current-id (atom 0))

(defn btw
  "Prompt the model with empty context.
   Convinient when you don't want to spoil
   conversation context with unrelated question.
   Args, are:
   query - the prompt
   context - optional, atom keeping model interaction state
             when omitted, *current* is used."
  ([query] (btw *current* query))
  ([context query]
   (let [topic (keyword (str "btw-" (swap! btw-current-id inc)))]
     (p context topic query))))


(defn ensure-mcp-servers-started
  "Ensure that all MCP servers started.
   context - optional, atom keeping model interaction state
             when omitted, *current* is used."
  ([] (ensure-mcp-servers-started *current*))
  ([context]
   (doseq [{:keys [name] :as srv-cfg} (get-in @context [:config :servers])]
     (when-not (get-in @context [:servers name])
       (let [srv (init-server srv-cfg)]
         (call-server srv
                      "initialize"
                      {:protocolVersion "2024-05-01"
                       :capabilities {} 
                       :clientInfo {:name "Quasi una fantasia"
                                    :version "0.16"}})
         (call-server srv "notifications/initialized" {})
         (swap! context update :servers assoc name srv)
         (let [tool-list (get-in (call-server srv "tools/list" {})
                                 [:result :tools])
               tools (for [tool tool-list]
                       {:type "function"
                        :function {:name (str name "__" (:name tool))
                                   :description (:description tool)
                                   :parameters (:inputSchema tool)}})
               dispatch (into {} (for [{tool-name :name} tool-list]
                                   [(str name "__" tool-name) [name tool-name]]))]
           (swap! context update :tools concat tools)
           (swap! context update :dispatch merge dispatch)))))))

(defn stop-mcp-servers
  "Stop all MCP servers.
   context - optional, atom keeping model interaction state
             when omitted, *current* is used."
  ([] (stop-mcp-servers *current*))
  ([context]
   (doseq [[name srv] (:servers @context)]
     (close-server srv)
     (swap! context update :servers dissoc name))
   (swap! context dissoc :tools)
   (swap! context dissoc :dispatch)
   nil))

(defn tools
  "List tools available in started MCP servers.
   context - optional, atom keeping model interaction state
             when omitted, *current* is used."
  ([] (tools *current*))
  ([context]
   (ui/table
    [:name :params :description]
    (for [{{fun :name
            {params :properties} :parameters
            desc :description} :function} (:tools @context)]
      {:name fun
       :params (->> params
                    keys
                    (map name)
                    (s/join ", "))
       :description desc}))))

(defn servers
  "List configured MCP servers.
   context - optional, atom keeping model interaction state
             when omitted, *current* is used."
  ([] (servers *current*))
  ([context]
   (ui/table
    [:name :started :cmd]
    (for [srv (get-in @context [:config :servers])]
      (assoc srv :started (when (get-in @context [:servers (:name srv)])
                            "yes"))))))

(defn- cut-content [subj]
  (when (not (nil? subj))
    (s/replace (if (> (count subj) 80)
                 (subs subj 0 80)
                 subj) #"\n" " ")))

(defn- render-tool-calls [subj]
  (->> subj
       (map :function)
       (map (fn [{:keys [name arguments]}]
              (str name "(" (->> (json/parse-string arguments)
                                 (map (fn [[k v]] (str k ": " v)))
                                 (s/join ", ")) ")")))
       (s/join "; ")))

(defn- render-log-entry [subj]
  (let [{:keys [content tool_calls role] :as all} subj]
    (assoc all :content (if tool_calls
                          (ui/html [:span.quf-keyword
                                    (render-tool-calls tool_calls)])
                          (if (= role "tool")
                            (ui/html [:span.quf-string
                                      (cut-content content)])
                            (cut-content content)))
               )))

(defn log
  "Show conversation logs. The log also contains MCP server interactions.
   Args are:
   query - the prompt
   context - optional, atom keeping model interaction state
             when omitted, *current* is used.
   topic - optional, a keyword representing a discussion topic
           topic holds separate context"
  ([] (log *current*))
  ([arg] (if (keyword? arg)
           (log *current* arg)
           (ui/table
            [:idx :topic :role :content]
            (map render-log-entry
                 (apply concat (for [[topic recs] (:messages @arg)]
                                 (map-indexed #(assoc %2 :topic topic :idx %1)
                                              recs)))))
           ))
  ([context topic]
   (ui/table
    [:idx :role :content]
    (map render-log-entry
         (map-indexed #(assoc %2 :idx %1)
                      (get-in @context [:messages topic]))))))
