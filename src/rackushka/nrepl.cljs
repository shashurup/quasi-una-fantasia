(ns rackushka.nrepl
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
    (with-meta [tag arg] {:rackushka/hint :tag})))

(defn read-value [subj]
  (try
    (binding [cljs.tools.reader/*default-data-reader-fn* handle-tag]
      (read-string subj))
    (catch js/Object _
      ^{:rackushka/hint :rackushka/text} [subj])))

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
                 (map read-value))}))

(defn update-ns [resp]
  (when-let [ns (some :ns resp)]
    (swap! state assoc :ns ns)))

(defn history-append [expr]
  (when-not (empty? expr)
    (swap! state update :history #(conj % expr))))

(defn update-session [resp]
  (swap! state assoc :session (:new-session (first resp))))

(defn send-eval [expr callback]
  (let [session (:session @state)]
    (if session
      (send-op {:op "eval"
                :code expr
                :session session
                :nrepl.middleware.print/print "rackushka.srv/pr-with-meta"}
               (fn [resp]
                 (history-append expr)
                 (update-ns resp)
                 (callback (merge-eval-result resp))))
      (send-op {:op "clone"}
               (fn [resp]
                 (update-session resp)
                 (send-eval expr callback))))))

(defn send-completions [text callback]
  (send-op {:op "completions"
            :prefix text
            :session (:session @state)}
           #(callback (:completions (first %)))))
