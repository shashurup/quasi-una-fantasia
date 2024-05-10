(ns shashurup.quf.nrepl
  (:require
   [goog.events :as gevents]
   [cljs.tools.reader :refer [read-string]]))

(defonce state (atom {}))

(defn get-ns [] (:ns @state))

(defn get-session [] (:session @state))

(defn- handle-tag [tag arg]
  (condp = tag
    'inst (js/Date. arg)
    (with-meta [tag arg] {:shashurup.quf/hint :tag})))

(defn try-read-value-with-meta [subj]
  (when (= (first subj) "^")
    (try
      (read-string subj)
      (catch js/Object _))))

(defn read-value [subj]
  (try
    (binding [cljs.tools.reader/*default-data-reader-fn* handle-tag]
      (read-string subj))
    (catch js/Object _
      ^{:shashurup.quf/hint :shashurup.quf/text} [subj])))

(defn history-append [expr]
  (when-not (empty? expr)
    (swap! state update :history #(conj % expr))))

(defn build-query-string [params]
  (let [query (str (js/URLSearchParams. (clj->js (or params {}))))]
    (if (empty? query)
      query
      (str "?" query))))

(defn new-id []
  (:cur-id (swap! state update :cur-id #(inc (or % 0)))))

(defn add-callback [id callback]
  (when id
    (first (swap-vals! state assoc-in [:callbacks id] callback))))

(defn remove-callback [id]
  (when id
    (swap! state update :callbacks dissoc id)))

(defn pending-callbacks? []
  (> (count (:callbacks @state)) 0))

(defn get-callback [id]
  (when id
    (get-in @state [:callbacks id])))

(defn terminated? [statuses]
  (some #{:done "done"} statuses))

(defn handle-response [req op callback]
  (when callback
    (callback (if (= 200 (.-status req))
                (read-string (.-responseText req))
                [(merge  {:status #{:done}
                          :err (.-responseText req)}
                         (select-keys op [:id]))]))))

(defn send-message [op callback & {:keys [timeout wait-reply] :as params}]
  (let [msg (pr-str op)
        req (js/XMLHttpRequest.)]
    (.open req "POST" (str "messages" (build-query-string params)))
    (.setRequestHeader req "Content-Type" "application/octet-stream")
    (gevents/listen req "loadend" #(handle-response req op callback))
    (.send req msg)))

(defn receive-messages [callback & {:keys [timeout] :as params}]
  (let [req (js/XMLHttpRequest.)]
    (.open req "GET" (str "messages" (build-query-string params)))
    (gevents/listen req "loadend" #(handle-response req nil callback))
    (.send req)))

(defn handle-replies [replies]
  (doall (map (fn [{:keys [id status] :as reply}]
                (when-let [callback (get-callback id)]
                  (try
                    (callback reply)
                    (catch js/Object ex
                      (. js/console error ex))))
                (when (and id (terminated? status))
                  (remove-callback id)))
              replies))
  (when (pending-callbacks?)
    (receive-messages handle-replies)))

(defn send-op
  ([op callback] (send-op op callback :session))
  ([op callback slot]
   (let [id (new-id)
         op (-> op
                (assoc :id id)
                (merge {:session (slot @state)}))]
     (if (empty? (:callbacks (add-callback id callback)))
       (send-message op handle-replies :wait-reply 1)
       (send-message op nil))
     id)))

(defn send-clone
  ([callback] (send-clone callback :session))
  ([callback slot]
   (send-op {:op "clone"}
            (fn [{:keys [new-session]}]
              (swap! state assoc slot new-session)
              (when callback
                (callback new-session))))))

(defn read-values [subj keys]
  (reduce (fn [m k]
            (if (contains? m k)
              (update m k read-value)
              m)) subj keys))

(defn send-eval
  ([expr callback] (send-eval expr callback nil))
  ([expr callback extra]
   (send-op (merge {:op "eval"
                    :code expr
                    :nrepl.middleware.print/print
                      "shashurup.quf.response/pr-with-meta"}
                   extra)
            (fn [reply]
              (let [reply (read-values reply [:value :x-data])]
                (when (terminated? (:status reply))
                  (history-append expr))
                (when-let [ns (:ns reply)]
                  (swap! state assoc :ns ns))
                (when callback
                  (callback reply)))))))

(defn send-eval-aux [expr callback]
  (if (:aux-session @state)
    (send-op {:op "eval"
              :code expr} callback :aux-session)
    (send-clone (fn [_]
                  (send-op {:op "eval"
                            :code "(require 'clojure.repl)"}
                           (fn [{status :status}]
                             (when (terminated? status)
                               (send-eval-aux expr callback)))
                           :aux-session))
                :aux-session)))

(defn send-interrupt [id]
  (send-op {:op "interrupt"} nil))

(defn send-completions [text callback]
  (send-op {:op "completions"
            :prefix text
            :options {:extra-metadata [:arglists]}}
           #(callback (:completions %))))

(defn send-history [terms limit callback]
  (send-op {:op "history"
            :terms terms
            :limit limit}
           #(callback (:matches %))))

(defn send-update-vars [updates]
  (send-op {:op "update-vars"
            :updates (pr-str updates)} nil))
