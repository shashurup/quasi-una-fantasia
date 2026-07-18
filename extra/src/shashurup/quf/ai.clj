(ns shashurup.quf.ai
  (:require [clojure.string :as s]
            [clj-http.client :as http]
            [shashurup.quf.secrets :as secrets]
            [shashurup.quf.view :as v]
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

(defn- call-tool [dispatch servers fun args]
  (let [[server-name name] (dispatch fun)
        srv (servers server-name)]
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

(defn- call-tools [dispatch servers topic subj tools-callback]
  (for [{{:keys [name arguments]
          :as tc} :function
         id :id} subj]
    (do
      (when tools-callback
        (tools-callback tc))
      (let [r (call-tool dispatch servers name arguments)]
        {:role "tool"
         :tool_call_id id
         :content (extract-tool-text r)}))))

(defn- ensure-system-message [context topic]
  (let [messages (get-in context [:messages topic])
        {{config-message :message} :config} context]
    (if messages
      context
      (assoc-in context
                [:messages topic] [{:role "system"
                                    :content [(or config-message
                                                  *default-message*)]}]))))

(defn- render-tool-call [{:keys [name arguments]}]
  (str name "(" (->> (json/parse-string arguments)
                     (map (fn [[k v]]
                            (str k "=" (pr-str v))))
                     (s/join ", ")) ");"))

(defn- split-lines [subj]
  (cond-> subj
    (:content subj) (update :content s/split-lines)
    (:reasoning subj) (update :reasoning s/split-lines)))

(defn- join-lines [subj]
  (cond-> subj
      (:content subj) (update :content #(s/join "\n" %))
      (:reasoning subj) (update :reasoning #(s/join "\n" %))))

(defn interact
  "Perform model interaction cycle.
   With tools call if necessary etc."
  [context topic tools-callback]
  (let [{{host :host
          key :key
          endpoint :endpoint
          model :model} :config
         tools :tools
         ms :messages} context
        messages (map #(select-keys % [:content :role :name :refusal
                                       :tool_calls :tool_call_id])
                      (get ms topic))
        key (if (map? key) (secrets/lookup key) key)]
    (let [resp (query-model host
                            (or endpoint *default-endpoint*)
                            key
                            model
                            (map join-lines messages)
                            tools)
          message (-> resp :choices first :message split-lines)
          {pt :prompt_tokens
           ct :completion_tokens} (:usage resp)
          context (-> context
                      (update-in [:messages topic] conj message)
                      (update :usage assoc
                              :prompt_tokens pt :completion_tokens ct)
                      (update-in [:usage :prompt_tokens_total] (fnil + 0) pt)
                      (update-in [:usage :completion_tokens_total] (fnil + 0) ct))]
      (if-let [tool_calls (:tool_calls message)]
        (let [tools_result (call-tools (:dispatch context)
                                       (:servers context)
                                       topic
                                       tool_calls
                                       tools-callback)
              context (update-in context
                                 [:messages topic]
                                 into
                                 (map split-lines tools_result))]
          (interact context topic tools-callback))
        context))))

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

(defn- perform-query [context topic query callback]
  (swap! context dissoc :exception)
  (swap! context assoc :log ["Ensure MCP servers started"])
  (when callback (callback))
  (ensure-mcp-servers-started context)
  (let [entry (str "Querying " (get-in @context [:config :model]))
        tools-callback (fn [subj]
                         (let [entry (render-tool-call subj)]
                           (swap! context update :log conj entry)
                           (when callback (callback))))]
    (swap! context update :log conj entry)
    (when callback (callback))
    (try
      (reset! context (-> @context
                          (ensure-system-message topic)
                          (update-in [:messages topic]
                                     conj {:role "user"
                                           :content (s/split-lines query)})
                          (interact topic tools-callback)))
      (catch Exception ex
        (swap! context assoc :exception ex))
      (finally (swap! context dissoc :log)))))

(defn fork
  ([] (fork *current*))
  ([context] (atom (select-keys @context [:config]))))

(defn- view-with-reason [content reasoning]
  (if reasoning
    (v/pack [content
             (v/details "Reasoning"
                        (v/hint reasoning :markdown))])
    content))

(defn status
  ([] (status @*current* :default))
  ([context] (status context :default))
  ([context topic]
   (if-let [ex (:exception context)]
     (v/highlight (ex-message ex) "error")
     (if-let [log (:log context)]
       (v/text (take-last 16 log))
       (let [{:keys [content
                     reasoning]} (-> context
                                     (get-in [:messages topic])
                                     last)]
         (view-with-reason (v/hint content :markdown)
                           reasoning))))))

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
   nil))

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
   (perform-query context topic query
                  #(v/report-progress (v/text (:log @context))))
   (status @context topic)))

(defn sp
  "Initiate model interactions. Args are:
   query - the prompt
   context - optional, atom keeping model interaction state
             when omitted, *current* is used.
   topic - optional, a keyword representing a discussion topic
           topic holds separate context"
  ([query] (sp *current* :default query))
  ([arg query] (if (keyword? arg)
                 (sp *current* arg query)
                 (sp arg :default query)))
  ([context topic query]
   (future
     (perform-query context topic query nil))))

(defonce btw-current-id (atom 0))

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

(defn btww
  "Prompt the model with the context of previous btw.
   Args, are:
   query - the prompt
   context - optional, atom keeping model interaction state
             when omitted, *current* is used."
  ([query] (btww *current* query))
  ([context query]
   (let [topic (keyword (str "btw-" @btw-current-id))]
     (p context topic query))))


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
   (v/table
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
   (v/table
    [:name :started :cmd]
    (for [srv (get-in @context [:config :servers])]
      (assoc srv :started (when (get-in @context [:servers (:name srv)])
                            "yes"))))))

(defn- render-tool-calls [subj]
  (->> subj
       (map :function)
       (map render-tool-call)))

(defn- render-log-entry [subj]
  (let [{:keys [content tool_calls role reasoning] :as all} subj
        content (if tool_calls
                  (render-tool-calls tool_calls)
                  content)
        summary (if (> (count content) 1)
                  (first content)
                  (if reasoning
                    (first content)))
        color (cond tool_calls      "symbol"
                    (= role "user") "keyword"
                    (= role "tool") "class")
        build (cond tool_calls      #(v/code % "js")
                    (= role "tool") v/text
                    :else           #(v/hint % :markdown))]
    (assoc all :content
           (if summary
             (v/details (v/highlight summary color)
                        (view-with-reason (build content)
                                          reasoning))
             (v/highlight (first content) color)))))

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
           (v/table
            [:idx :topic :role :content]
            (with-meta
              (map render-log-entry
                   (apply concat (for [[topic recs] (:messages @arg)]
                                   (map-indexed #(assoc %2 :topic topic :idx %1)
                                                recs))))
              ;; Just a hack - tables dont expect complex cell content
              {:shashurup.quf/range {:more? false}}))))
  ([context topic]
   (v/table
    [:idx :role :content]
    (map render-log-entry
         (map-indexed #(assoc %2 :idx %1)
                      (get-in @context [:messages topic]))))))
