(ns shashurup.quf.ai
  (:require [clj-http.client :as http]
            [shashurup.quf.secrets :as secrets]
            [shashurup.quf.response :as r]
            ))

(def ^:dynamic *default-endpoint* "/api/v1/chat/completions")
(def ^:dynamic *default-message* "You are a large language model and a helpful assistant.
                                  Respond concisely.")

(defn- query-model [host endpoint key model messages]
  (:body (http/post (str host endpoint)
                    {:headers {"Authorization" (str "Bearer " key)}
                     :content-type :json
                     :as :json
                     :form-params {:model model
                                   :messages messages}})))

(defn interact
  "Prompt a model"
  [context topic query]
  (let [{{host :host
          key :key
          endpoint :endpoint
          message :message
          model :model} :config} @context
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
                            (get-in @context [:messages topic]))
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
