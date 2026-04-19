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
(def ^:dynamic *default-message* "You are a large language model and a helpful assistant.
                                  Respond concisely.")

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
      {:server server
       :type "function"
       :function {:name (str server "." (:name tool))
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
  (let [[srv {:keys [in out]}] (->> @context
                                    :servers
                                    (filter #(s/starts-with? fun (first %)))
                                    first)
        name (subs fun (inc (count srv)))
        args (json/parse-string args)]
    (call-mcp-server in out "tools/call" {:name name
                                          :arguments args})))

(defn- call-tools [context topic subj]
  (append-message! context
                   topic
                   {:role "assistant"
                    :tools_call subj})
  (doseq [{{fun :name
            args :arguments} :function
           id :id} subj]
    (let [r (call-tool context id fun args)]
      (append-message! context
                       topic
                       {:role "tool"
                        :content (json/generate-string r)
                        :tool_call_id id}))))

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

(defn- ensure-system-message! [context topic]
  (swap! context
         #(if (get-in % [:messages topic])
            %
            (assoc-in % [:messages topic]
                      [{:role "system"
                        :content (or (get-in % [:config :message])
                                     *default-message*)}]))))

(defn- append-message! [context topic message]
  (swap! context update-in [:messages topic] conj message))

(defn interact
  "Prompt a model"
  [context topic query]
  (let [{{host :host
          key :key
          endpoint :endpoint
          message :message
          model :model} :config
         tools :tools} @context
        key (if (map? key) (secrets/lookup key) key)]
    (ensure-system-message! context topic)
    (when query
      (append-message! context topic {:role "user"
                                      :content query}))
    (let [resp (query-model host
                            (or endpoint *default-endpoint*)
                            key
                            model
                            (get-in @context [:messages topic])
                            tools)
          choice (-> resp :choices first)
          content (get-in choice [:message :content])
          tool_calls (get-in choice [:message :tool_calls])]
      (cond content (do
                      (append-message! context
                                       topic
                                       {:role "assistant"
                                        :content content})
                      choice)
            tool_calls (do
                         (call-tools context topic tool_calls)
                         (interact context topic nil))))))

(def ^:dynamic *current*)

(defn c [subj]
  (if (map? subj)
    (def ^:dynamic *current* (atom {:config subj}))
    (def ^:dynamic *current* subj)))

(defn clear
  ([] (clear *current*))
  ([arg] (if (keyword? arg)
           (clear *current* arg)
           (swap! arg dissoc :messages)))
  ([context topic] (swap! context
                          update :messages
                          dissoc topic)))

(defn p
  ([query] (p *current* :default query))
  ([arg query] (if (keyword? arg)
                 (p *current* arg query)
                 (p arg :default query)))
  ([context topic query]
   (-> (interact context topic query)
       (get-in [:message :content])
       vector
       (r/hint :markdown))))

(defn btw
  ([query] (btw *current* query))
  ([context query]
   (let [temp-context (atom {:config (:config @context)})]
     (p temp-context :temp query))))


(defn ensure-mcp-servers-started
  ([] (ensure-mcp-servers-started *current*))
  ([context]
   (doseq [{:keys [name cmd]} (get-in @context [:config :servers])]
     (when-not (get-in @context [:servers name])
       (let [{:keys [in out] :as proc} (init-mcp-server cmd)
             tools (list-mcp-tools in out name)]
         (swap! context update :servers assoc name proc)
         (swap! context update :tools concat tools))))))

(defn stop-mcp-servers
  ([] (stop-mcp-servers *current*))
  ([context]
   (doseq [[name {:keys [out process]}] (:servers @context)]
     (shutdown-mcp-server out process)
     (swap! context update :servers dissoc name))
   (swap! context dissoc :tools)))

(defn tools
  ([] (tools *current*))
  ([context]
   (ui/table
    [:name :description]
    (for [{{name :name
            desc :description} :function} (:tools @context)]
      {:name name
       :description desc}))))

(defn servers
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

(defn log
  ([] (log *current*))
  ([arg] (if (keyword? arg)
           (log *current* arg)
           (ui/table
            [:topic :role :content :tools_call]
            (map #(update % :content cut-content)
                 (apply concat (for [[topic recs] (:messages @arg)]
                                 (map #(assoc % :topic topic) recs)))))
           ))
  ([context topic]
   (ui/table
    [:role :content :tools_call]
    (map #(update % :content cut-content)
         (get-in @context [:messages topic])))))
