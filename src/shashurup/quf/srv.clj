(ns shashurup.quf.srv
  (:gen-class)
  (:require [shashurup.quf.response :as response]
            [shashurup.quf.vars :as vars]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.coercions :refer [as-int]]
            [ring.middleware.defaults :as d]
            [ring.adapter.jetty :as j]
            [ring.util.response :as u]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [nrepl.transport :as t]
            [nrepl.server :as srv]))

(defn create-transport []
  (let [[client server] (t/piped-transports)]
    (future (srv/handle (srv/default-handler #'vars/wrap-update-vars)
                        server))
    client))

(defn parse-message [body]
  (-> body
      io/reader
      java.io.PushbackReader.
      edn/read))

(defn response-seq [transport timeout]
  (loop [msg (t/recv transport (* 1000 timeout))
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
        timeout (or (as-int timeout) 8)]
    (when (= method :post)
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

(defn start-server [join?]
  (j/run-jetty (d/wrap-defaults app
                                (-> d/site-defaults
                                    ;; site defaults uses cookie-store
                                    ;; which we don't need
                                    (update :session dissoc :store)
                                    (update :security dissoc :anti-forgery)))
               {:port 9500 :join? join?}))

(defn -main [& args]
  (start-server true))
