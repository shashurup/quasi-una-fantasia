(ns shashurup.quf.nrepl
  (:require
   [goog.events :as gevents]
   [cljs.tools.reader :refer [read-string]]))

(defn- local-id []
  (.toString (rem (js/Date.now) 10000000) 36))

(defonce state (atom {:bg-events #{}
                      :client-id (local-id)}))

(defn get-ns [] (:ns @state))

(defn- handle-tag [tag arg]
  (condp = tag
    'inst (js/Date. arg)
    (with-meta [tag arg] {:shashurup.quf/hint :tag})))

(defn- read-value [subj]
  (try
    (binding [cljs.tools.reader/*default-data-reader-fn* handle-tag]
      (read-string subj))
    (catch js/Object _
      ^{:shashurup.quf/hint :text} [subj])))

(defn- history-append [expr]
  (when-not (empty? expr)
    (swap! state update :history #(conj % expr))))

(defn- build-query-string [params]
  (let [query (str (js/URLSearchParams. (clj->js (or params {}))))]
    (if (empty? query)
      query
      (str "?" query))))

(defn- new-id []
  (:cur-id (swap! state update :cur-id #(inc (or % 0)))))

(defn- add-callback [id callback]
  (when id
    (first (swap-vals! state assoc-in [:callbacks id] callback))))

(defn- remove-callback [id]
  (when id
    (swap! state update :callbacks dissoc id)))

(defn- add-bg-event [id]
  (when id
    (swap! state update :bg-events conj id)))

(defn- remove-bg-event [id]
  (when id
    (swap! state update :bg-events disj id)))

(defn- no-bg-event [id]
  (not ((:bg-events @state) id)))

(defn- pending-callbacks? []
  (> (count (:callbacks @state)) 0))

(defn- get-callback [id]
  (when id
    (get-in @state [:callbacks id])))

(defn terminated? [statuses]
  (some #{:done "done"} statuses))

(defn- handle-http-response [req op callback]
  (if (= 401 (.-status req))
    (.assign (.-location js/window) "login")
    (when callback
      (callback (if (= 200 (.-status req))
                  (read-string (.-responseText req))
                  [(merge  {:status #{:done}
                            :err (.-responseText req)}
                           (select-keys op [:id]))])))))

(defn- send-http-message [op callback & {:keys [timeout wait-reply] :as params}]
  (let [msg (pr-str op)
        req (js/XMLHttpRequest.)
        params (assoc params :client-id (:client-id @state))]
    (.open req "POST" (str "messages" (build-query-string params)))
    (.setRequestHeader req "Content-Type" "application/octet-stream")
    (gevents/listen req "loadend" #(handle-http-response req op callback))
    (.send req msg)))

(defn- receive-http-messages [callback & {:keys [timeout] :as params}]
  (let [req (js/XMLHttpRequest.)
        params (assoc params :client-id (:client-id @state))]
    (.open req "GET" (str "messages" (build-query-string params)))
    (gevents/listen req "loadend" #(handle-http-response req nil callback))
    (.send req)))

(defn- process-callback [{:keys [id status] :as reply}]
  (when-let [callback (get-callback id)]
    (try
      (callback reply)
      (catch js/Object ex
        (. js/console error ex))))
  (when (:expect-background-events status)
    (add-bg-event id))
  (when (:no-more-background-events status)
    (remove-bg-event id)
    (remove-callback id))
  (when (and id (terminated? status) (no-bg-event id))
    (remove-callback id)))

(defn- handle-http-replies [replies]
  (doall (map process-callback replies))
  (when (pending-callbacks?)
    (receive-http-messages handle-http-replies)))

(defn- send-op-http
  ([op callback] (send-op-http op callback :session))
  ([op callback slot]
   (let [id (new-id)
         op (-> op
                (assoc :id id)
                (merge {:session (slot @state)}))]
     (if (empty? (:callbacks (add-callback id callback)))
       (send-http-message op handle-http-replies :wait-reply 1)
       (send-http-message op nil))
     id)))

(defn- on-ws-message [event]
  (process-callback (read-string (.-data event))))

(defn- ws-connect [callback]
  (let [ws (js/WebSocket. (str "ws?client-id="
                               (:client-id @state)))]
    (.addEventListener ws "open" #((swap! state assoc :socket ws)
                                   (when callback
                                     (callback))))
    (.addEventListener ws "close" #(if (= :connecting (:socket @state))
                                     (do
                                       (swap! state assoc :socket :missing)
                                       (when callback
                                         (callback)))
                                     (do
                                       (swap! state dissoc :socket)
                                       (when (pending-callbacks?)
                                         (ws-connect nil)))))
    (.addEventListener ws "message" on-ws-message)
    (.addEventListener ws "error" #(.log js/console "WebSocket error:" %))
    (swap! state assoc :socket :connecting)))

(declare send-op)

(defn- send-op-ws [op callback slot]
  (let [sock (:socket @state)]
    (cond
      (nil? sock) (ws-connect #(send-op op callback slot))
      (= sock :connecting) (js/setTimeout #(send-op op callback slot))
      :else (let [id (new-id)
                  op (-> op
                         (assoc :id id)
                         (merge {:session (slot @state)}))]
              (add-callback id callback)
              (.send sock (pr-str op))
              id))))

(defn send-op
  ([op callback] (send-op op callback :session))
  ([op callback slot]
   (if (= (:socket @state) :missing)
     (send-op-http op callback slot)
     (send-op-ws op callback slot))))

(defn send-clone
  ([callback] (send-clone callback :session))
  ([callback slot]
   (send-op {:op "clone"}
            (fn [{:keys [new-session]}]
              (swap! state assoc slot new-session)
              (when callback
                (callback new-session))))))

(defn- read-values [subj keys]
  (reduce (fn [m k]
            (if (contains? m k)
              (update m k read-value)
              m)) subj keys))

(defn- serialize-code [code]
  (if (string? code)
    code
    (pr-str code)))

(defn send-eval
  ([expr callback] (send-eval expr callback nil))
  ([expr callback extra]
   (send-op (merge {:op "eval"
                    :code (serialize-code expr)
                    :nrepl.middleware.print/print
                      "shashurup.quf.view/pr-with-meta"}
                   extra)
            (fn [reply]
              (let [reply (read-values reply [:value :x-data :event])]
                (when (terminated? (:status reply))
                  (history-append (serialize-code expr)))
                (when-let [ns (:ns reply)]
                  (swap! state assoc :ns ns))
                (when callback
                  (callback reply)))))))

(defn send-eval-aux [expr callback]
  (if (:aux-session @state)
    (send-op {:op "eval"
              :ns (get-ns)
              :code (serialize-code expr)}
             (fn [reply]
               (when callback
                 (callback (read-values reply [:value]))))
             :aux-session)
    (send-clone (fn [_]
                  (send-op {:op "eval"
                            :code "(require 'clojure.repl)"}
                           (fn [{status :status}]
                             (when (terminated? status)
                               (send-eval-aux expr callback)))
                           :aux-session))
                :aux-session)))

(defn send-interrupt [id]
  (send-op {:op "interrupt"
            :interrupt-id id} nil))

(defn send-cancel-events [id]
  (send-op {:op "cancel-events"
            :cancel-id id} nil))

(defn send-completions [text callback]
  (send-op {:op "completions"
            :prefix text
            :options {:extra-metadata [:arglists]}}
           #(callback (:completions %))))

(defn send-update-vars [updates]
  (send-op {:op "update-vars"
            :updates (pr-str updates)} nil))
