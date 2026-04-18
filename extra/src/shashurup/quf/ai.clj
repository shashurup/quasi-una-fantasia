(ns shashurup.quf.ai
  (:require [clj-http.client :as http]
            [shashurup.quf.secrets :as secrets]
            [shashurup.quf.response :as r]
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

(defn- list-mcp-tools [in out]
  (let [mcp-tools (call-mcp-server in out "tools/list")]
    (for [tool (get-in mcp-tools [:result :tools])]
      {:type "function"
       :function {:name (:name tool)
                  :description (:description tool)
                  :parameters (:inputSchema tool)}})))

(defn- shutdown-mcp-server [out p]
  (.close out)
  (try
    (when-not (.waitFor p 5 TimeUnit/SECONDS)
      (.destroyForcibly p))
    (catch InterruptedException e
      (.destroyForcibly p))))

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
    (swap! context
           #(if (get-in % [:messages topic])
              %
              (assoc-in % [:messages topic]
                        [{:role "system"
                          :content (or message *default-message*)}])))
    (swap! context
           update-in [:messages topic]
           conj {:role "user"
                 :content query})
    (let [resp (query-model host
                            (or endpoint *default-endpoint*)
                            key
                            model
                            (get-in @context [:messages topic])
                            tools)
          choice (-> resp :choices first)]
      (swap! context
             update-in [:messages topic]
             conj {:role "assistant"
                   :content (get-in choice [:message :content])})
      choice)))

(def ^:dynamic *current*)

(defn c [subj]
  (if (map? subj)
    (def ^:dynamic *current* (atom {:config subj}))
    (def ^:dynamic *current* subj)))

(defn clear
  ([] (clear *current*))
  ([context] (swap! context dissoc :messages)))

(defn p
  ([query] (p *current* :default query))
  ([topic query] (p *current* topic query))
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
             tools (list-mcp-tools in out)]
         (swap! context update :servers assoc name proc)
         (swap! context update :tools concat tools))))))

(defn stop-mcp-servers
  ([] (stop-mcp-servers *current*))
  ([context]
   (doseq [[name {:keys [out process]}] (:servers @context)]
     (shutdown-mcp-server out process)
     (swap! context update :servers dissoc name))
   (swap! context dissoc :tools)))
