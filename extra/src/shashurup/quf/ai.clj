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

(defn- call-mcp-server
  ([in out method] (call-mcp-server in out method {}))
  ([in out method params]
   (let [msg (json/generate-string {:jsonrpc "2.0"
                                    :method method
                                    :params params
                                    :id 1})]
     (.write out (str msg "\n"))
     (.flush out)
     (json/parse-string (.readLine in) true))))

(defn- start-mcp-server [args]
  (let [p (.start (ProcessBuilder. args))]
    {:in (BufferedReader. (InputStreamReader. (.getInputStream p)))
     :err (BufferedReader. (InputStreamReader. (.getErrorStream p)))
     :out (OutputStreamWriter. (.getOutputStream p))
     :process p}))

(defn- notify-client-ready [out]
  (let [msg (json/generate-string {:jsonrpc "2.0"
                                   :method "notifications/initialized"})]
    (.write out (str msg "\n"))))

(defn- init-mcp-server [args]
  (let [{:keys [in out] :as result} (start-mcp-server args)]
    (let [resp (call-mcp-server in out "initialize"
                                {:protocolVersion "2024-05-01"
                                 :capabilities {} 
                                 :clientInfo {:name "Quasi una fantasia"
                                              :version "0.16"}})]
      (notify-client-ready out)
      result)))

(defn- list-mcp-tools [in out server]
  (let [mcp-tools (call-mcp-server in out "tools/list")]
    (for [tool (get-in mcp-tools [:result :tools])]
      {:type "function"
       :function {:name (str server "_" (:name tool))
                  :description (:description tool)
                  :parameters (:inputSchema tool)}})))

(defn- shutdown-mcp-server [out p]
  (.close out)
  (try
    (when-not (.waitFor p 5 TimeUnit/SECONDS)
      (.destroyForcibly p))
    (catch InterruptedException e
      (.destroyForcibly p))))

(defn- call-tool [context id fun args]
  (let [[srv {:keys [in out]}] (->> context
                                    :servers
                                    (filter #(s/starts-with? fun (first %)))
                                    first)
        name (subs fun (inc (count srv)))
        args (json/parse-string args)]
    (call-mcp-server in out "tools/call" {:name name
                                          :arguments args})))

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
                      (let [r (call-tool context id fun args)]
                        {:role "tool"
                         :tool_call_id id
                         :content (extract-tool-text r)})))))

(defn ensure-system-message [context topic]
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

(defn clear
  "Clear model context. Args can be:
   context - optional, atom keeping model interaction state
             when omitted, *current* is used.
   topic - optional, a keyword representing a topic to clear"
  ([] (clear *current*))
  ([arg] (if (keyword? arg)
           (clear *current* arg)
           (swap! arg dissoc :messages)))
  ([context topic]
   (swap! context
          update :messages
          dissoc topic)
   topic))

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
   (let [temp-context (atom {:config (:config @context)})]
     (p temp-context :temp query))))


(defn ensure-mcp-servers-started
  "Ensure that all MCP servers started.
   context - optional, atom keeping model interaction state
             when omitted, *current* is used."
  ([] (ensure-mcp-servers-started *current*))
  ([context]
   (doseq [{:keys [name cmd]} (get-in @context [:config :servers])]
     (when-not (get-in @context [:servers name])
       (let [{:keys [in out] :as proc} (init-mcp-server cmd)
             tools (list-mcp-tools in out name)]
         (swap! context update :servers assoc name proc)
         (swap! context update :tools concat tools))))))

(defn stop-mcp-servers
  "Stop all MCP servers.
   context - optional, atom keeping model interaction state
             when omitted, *current* is used."
  ([] (stop-mcp-servers *current*))
  ([context]
   (doseq [[name {:keys [out process]}] (:servers @context)]
     (shutdown-mcp-server out process)
     (swap! context update :servers dissoc name))
   (swap! context dissoc :tools)))

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
                            (cut-content content))))))

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
            [:topic :role :content]
            (map render-log-entry
                 (apply concat (for [[topic recs] (:messages @arg)]
                                 (map #(assoc % :topic topic) recs)))))
           ))
  ([context topic]
   (ui/table
    [:role :content]
    (map render-log-entry
         (get-in @context [:messages topic])))))
