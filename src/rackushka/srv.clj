(ns rackushka.srv
  (:require [rackushka.config :as cfg]
            [rackushka.events :as ev]
            [rackushka.history :as hist]
            [compojure.core :refer :all]
            [compojure.route :as route]
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
                                                      #'ev/wrap-events)
                                 server-side))
     :client (nrepl/client client-side (* 24 60 60 1000))}))

(defn handle-nrepl-request [op client]
  (let [result (nrepl/message client op)]
    (hist/log op result)
    result))

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

  (GET "/fs/*" {{path :*} :params}
       (u/file-response (str "/" path)))

  (route/not-found "<h1>Page not found</h1>"))

(defn start-server [join?]
  (j/run-jetty (d/wrap-defaults app {:session true})
               {:port 9500 :join? join?}))

(defn -main [& args]
  (start-server true))
