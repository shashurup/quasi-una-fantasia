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
