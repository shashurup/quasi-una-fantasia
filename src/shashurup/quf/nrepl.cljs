(ns shashurup.quf.nrepl
  (:require
   [goog.events :as gevents]
   [cljs.tools.reader :refer [read-string]]))

(defonce state (atom {}))

(defn get-ns [] (:ns @state))

(defn send-op [op callback]
  (let [msg (pr-str op)
        req (js/XMLHttpRequest.)]
    (.open req "POST" "repl")
    (.setRequestHeader req "Content-Type" "application/octet-stream")
    (gevents/listen req
                    "loadend"
                    #(callback (if (= 200 (.-status req))
                                 (read-string (.-responseText req))
                                 (.-responseText req))))
    (.send req msg)))


(defn- handle-tag [tag arg]
  (condp = tag
    'inst (js/Date. arg)
    (with-meta [tag arg] {:shashurup.quf/hint :tag})))

(defn read-value [subj]
  (try
    (binding [cljs.tools.reader/*default-data-reader-fn* handle-tag]
      (read-string subj))
    (catch js/Object _
      ^{:shashurup.quf/hint :shashurup.quf/text} [subj])))

(defn merge-eval-result [subj]
  (if (string? subj)
    {:err subj}
    {:id (:id (first subj))
     :out (->> subj
               (map #(select-keys % [:out :err :ex :root-ex]))
               (remove empty?))
     :value (->> subj
                 (map :value)
                 (remove nil?)
                 (map read-value))
     :event-queue-size (->> subj
                            (map :event-queue-size)
                            (remove nil?)
                            first)}))

(defn update-ns [resp]
  (when-let [ns (some :ns resp)]
    (swap! state assoc :ns ns)))

(defn history-append [expr]
  (when-not (empty? expr)
    (swap! state update :history #(conj % expr))))

(defn update-session [resp]
  (swap! state assoc :session (:new-session (first resp))))

(defn send-eval
  ([expr callback] (send-eval expr callback nil))
  ([expr callback extra]
   (let [session (:session @state)]
     (if session
       (let [id (:cur-id (swap! state update :cur-id #(inc (or % 0))))
             op-args (merge {:op "eval"
                             :code expr
                             :session session
                             :id id
                             :nrepl.middleware.print/print "shashurup.quf.srv/pr-with-meta"}
                            extra)]
         (send-op op-args
                  (fn [resp]
                    (history-append expr)
                    (update-ns resp)
                    (callback (merge-eval-result resp))))
         id)
       (send-op {:op "clone"}
                (fn [resp]
                  (update-session resp)
                  (send-eval expr callback extra)))))))

(defn send-interrupt [id]
  (send-op {:op "interrupt"
            :session (:session @state)
            :id id}
           (fn [_])))

(defn send-completions [text callback]
  (send-op {:op "completions"
            :prefix text
            :session (:session @state)
            :options {:extra-metadata [:arglists]}}
           #(callback (:completions (first %)))))

(defn send-history [terms limit callback]
  (send-op {:op "history"
            :terms terms
            :limit limit
            :session (:session @state)}
           #(callback (:matches (first %)))))

;; =========== New REPL interactions =========================

(defn build-query-string [params]
  (let [query (str (js/URLSearchParams. (clj->js (or params {}))))]
    (if (empty? query)
      query
      (str "?" query))))

(defn new-id []
  (:cur-id (swap! state update :cur-id #(inc (or % 0)))))

(defn add-callback [id callback]
  (when id
    (swap! state assoc-in [:callbacks id] callback)))

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
  (when (not (:session @state))
    (swap! state assoc :session (:session (first replies))))
  (doall (map (fn [{:keys [id status] :as reply}]
                (when-let [callback (get-callback id)]
                  (callback reply))
                (when (and id (terminated? status))
                  (remove-callback id)))
              replies))
  (when (pending-callbacks?)
    (receive-messages handle-replies)))

(defn send-op2 [op callback]
  (let [already-waiting (pending-callbacks?)
        id (new-id)
        op (-> op
               (assoc :id id)
               (merge (select-keys @state [:session])))]
    (add-callback id callback)
    (if already-waiting
      (send-message op nil)
      (send-message op handle-replies :wait-reply 1))
    id))

(defn send-eval2
  ([expr callback] (send-eval2 expr callback nil))
  ([expr callback extra]
   (send-op2 (merge {:op "eval"
                     :code expr
                     :nrepl.middleware.print/print
                       "shashurup.quf.srv/pr-with-meta"}
                    extra)
             (fn [reply]
               (let [reply (if (:value reply)
                             (update reply :value read-value)
                             reply)]
                 (history-append expr)
                 (when-let [ns (:ns reply)]
                   (swap! state assoc :ns ns))
                 (when (and callback
                            (some #{:value :out :err :ex :root-ex}
                                  (keys reply)))
                   (callback reply)))))))
