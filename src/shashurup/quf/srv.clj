(ns shashurup.quf.srv
  (:require [shashurup.quf.config :as cfg]
            [shashurup.quf.events :as events]
            [shashurup.quf.history :as hist]
            [shashurup.quf.selection :as selection]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.coercions :refer [as-int]]
            [ring.middleware.defaults :as d]
            [ring.adapter.jetty :as j]
            [ring.util.response :as u]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [nrepl.transport :as t]
            [nrepl.core :as nrepl]
            [nrepl.server :as srv]))

(defn pr-with-meta [subj target _]
  (binding [*out* target
            *print-meta* true]
    (pr subj)))

(defn create-web-session []
  (let [[client-side server-side] (t/piped-transports)]
    {:server (future (srv/handle (srv/default-handler #'hist/wrap-history
                                                      #'events/wrap-events
                                                      #'cfg/wrap-config
                                                      #'selection/wrap-selection)
                                 server-side))
     :client (nrepl/client client-side (* 24 60 60 1000))
     ;; :transport client-side
     }))

(defn handle-nrepl-request [op client]
  (let [result (nrepl/message client op)]
    ;; TODO move this into the middleware
    ;; eval result can be accessed wrapping transport
    ;; pretty printing middleware does this
    (hist/log op result)
    (events/augment-response op result)))

(defn create-transport []
  (let [[client server] (t/piped-transports)]
    (future (srv/handle (srv/default-handler #'hist/wrap-history
                                             #'events/wrap-events
                                             #'cfg/wrap-config
                                             #'selection/wrap-selection)
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

  (POST "/repl" {session :session
                 body :body}
        (let [session (if (empty? session)
                        (create-web-session)
                        session)]
          (-> body
              io/reader
              java.io.PushbackReader.
              edn/read
              (handle-nrepl-request (:client session))
              pr-str
              u/response
              (assoc :session session))))

  (GET "/messages" req (handle-message req))
  
  (POST "/messages" req (handle-message req))
  
  (GET "/fs/*" {{path :*} :params}
       (u/file-response (str "/" path)))

  (route/not-found "<h1>Page not found</h1>"))

(defn start-server [join?]
  (j/run-jetty (d/wrap-defaults app {:session true})
               {:port 9500 :join? join?}))

(defn -main [& args]
  (start-server true))
