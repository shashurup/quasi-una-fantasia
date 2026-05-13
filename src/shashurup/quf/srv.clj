(ns shashurup.quf.srv
  (:gen-class)
  (:require [shashurup.quf.response :as response]
            [shashurup.quf.vars :as vars]
            [shashurup.quf.pruner :as pruner]
            [shashurup.quf.evt :as evt]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.coercions :refer [as-int]]
            [ring.middleware.defaults :as d]
            [ring.adapter.jetty :as j]
            [ring.util.response :as u]
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
                       {transport :transport} :session
                       method :request-method
                       body :body
                       {timeout :timeout
                        wait-reply :wait-reply} :params}]
  (let [transport (or transport (create-transport))
        timeout (or (as-int timeout) TIMEOUT)]
    (when (= method :post)
      ;; TODO handle timeout (in case something is running already)
      (t/send transport (parse-message body)))
    (let [r (when (or (= method :get) wait-reply)
              (response-seq transport timeout))]
      (-> r
          pr-str
          u/response
          (assoc :session (assoc session :transport transport))))))

(defroutes app

  (GET "/messages" req (handle-message req))
  
  (POST "/messages" req (handle-message req))
  
  (GET "/fs/*" {{path :*} :params}
       (u/file-response (str "/" path)))

  (GET "/" [] (->  (u/resource-response "index.html"
                                        {:root "public"})
                   (u/content-type "text/html; charset=utf-8")))
  
  (route/not-found "<h1>Page not found</h1>"))

(defn start
  ([port] (start port false))
  ([port join?]
   (j/run-jetty (d/wrap-defaults app
                                 (-> d/site-defaults
                                     ;; site defaults uses cookie-store
                                     ;; which we don't need
                                     (update :session dissoc :store)
                                     (update :security dissoc :anti-forgery)))
                {:port port :join? join?})))

(defn -main [& args]
  (let [port (if-let [p (first args)]
               (Integer/parseInt p)
               9500)]
    (start port true)))
