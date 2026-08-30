(ns shashurup.quf.srv
  (:gen-class)
  (:require [shashurup.quf.vars :as vars]
            [shashurup.quf.evt :as evt]
            [shashurup.quf.pruner :as pruner]
            [shashurup.quf.sweeper :as sweeper]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.coercions :refer [as-int]]
            [ring.middleware.defaults :as d]
            [ring.adapter.jetty :as j]
            [ring.util.response :as u]
            [ring.websocket :as ws]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [nrepl.transport :as t]
            [nrepl.server :as srv])
  (:import [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(def SERVER-QUEUE-CAPACITY 16)
(def TIMEOUT (* 16 1000))

(deftype QueueTransport [^ArrayBlockingQueue in ^ArrayBlockingQueue out]
  t/Transport
  (send [this msg]
    (when-not (.offer out msg TIMEOUT TimeUnit/MILLISECONDS)
      (throw (ex-info "Pipe timeout" {:reason :timeout})))
    this)
  (recv [_this] (.take in))
  (recv [_this timeout] (.poll in timeout TimeUnit/MILLISECONDS)))

;; TODO handle sessions expiration
(defn create-transport []
  (let [cq (ArrayBlockingQueue. 1)
        sq (ArrayBlockingQueue. SERVER-QUEUE-CAPACITY)
        client (QueueTransport. sq cq)
        server (QueueTransport. cq sq)]
    (future (srv/handle (srv/default-handler #'vars/wrap-update-vars
                                             #'pruner/wrap-pruner
                                             #'sweeper/wrap-sweeper
                                             #'evt/wrap-event-reporter)
                        server))
    client))

(defn parse-message [body]
  (-> body
      io/reader
      java.io.PushbackReader.
      edn/read))

(defn response-seq [transport timeout]
  (loop [msg (t/recv transport timeout)
         result []]
    (if msg
      ;; when evaluation result is read
      ;; wait for the next :status message
      (let [timeout (if (contains? msg :value) timeout 0)]
        (recur (t/recv transport (* 1000 timeout))
               (conj result msg)))
      result)))

(defn handle-message [{session :session
                       {transports :transports} :session
                       method :request-method
                       body :body
                       {timeout :timeout
                        wait-reply :wait-reply
                        client-id :client-id} :params}]
  (let [transport (or (get transports client-id)
                      (create-transport))
        timeout (or (as-int timeout) TIMEOUT)]
    (when (= method :post)
      ;; TODO handle timeout (in case something is running already)
      (t/send transport (parse-message body)))
    (let [r (when (or (= method :get) wait-reply)
              (response-seq transport timeout))]
      (-> r
          pr-str
          u/response
          (assoc :session (assoc-in session
                                    [:transports client-id]
                                    transport))))))

(deftype SendOnlyTransport [callback]
  t/Transport
  (send [this msg]
    (callback msg)
    this)
  (recv [_this])
  (recv [_this timeout]))

(def nrepl-handler (srv/default-handler #'vars/wrap-update-vars
                                        #'pruner/wrap-pruner
                                        #'sweeper/wrap-sweeper
                                        #'evt/wrap-event-reporter))

(defn process-message [msg callback]
  (srv/handle* msg
               nrepl-handler
               (SendOnlyTransport. callback)))

(defn connect-websocket [{session :session
                          {sockets :sockets} session
                          {client-id :client-id} :params}]
  (let [sockets (or sockets (atom {}))
        send-reply (fn [msg]
                     ;; TODO handle disconnected socket
                     ;; by queueing replies until limit
                     ;; and throwing
                     (ws/send (@sockets client-id)
                              (pr-str msg)))]
    {:session (assoc session :sockets sockets)
     ::ws/listener
     {:on-open (fn [sock]
                 (swap! sockets assoc client-id sock))
      :on-message (fn [sock msg]
                    (process-message (edn/read-string msg)
                                     send-reply))
      :on-close (fn [sock _ _]
                  (swap! sockets dissoc client-id))}}))

(def app
  (d/wrap-defaults
   (routes
    (GET "/messages" req (handle-message req))
    (POST "/messages" req (handle-message req))
    (GET "/ws" req (connect-websocket req))
    (GET "/fs/*" {{path :*} :params}
         (u/file-response (str "/" path)))
    (GET "/" [] (->  (u/resource-response "index.html"
                                          {:root "public"})
                     (u/content-type "text/html; charset=utf-8")))
    (route/not-found "<h1>Page not found</h1>"))
   (-> d/site-defaults
       ;; site defaults uses cookie-store
       ;; which we don't need
       (update :session dissoc :store)
       (update :security dissoc :anti-forgery))))

(defn start
  ([port] (start port false))
  ([port join?]
   (j/run-jetty app {:port port :join? join?})))

(defn -main [& args]
  (let [port (if-let [p (first args)]
               (Integer/parseInt p)
               9500)]
    (start port true)))
