(ns rackushka.srv
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :as d]
            [ring.adapter.jetty :as j]
            [ring.util.response :as u]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [nrepl.transport :as t]
            [nrepl.core :as nrepl]
            [nrepl.server :as srv]))

(defn create-web-session []
  (let [tr (t/piped-transports)]
    {:server (future (srv/handle (srv/default-handler)
                                 (second tr)))
     :client (nrepl/client (first tr) 5000)}))

(defn handle-nrepl-request [op client]
  (nrepl/message client op))

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

  (route/not-found "<h1>Page not found</h1>"))

(defn start-server [join?]
  (j/run-jetty (d/wrap-defaults app {:session true})
               {:port 9500 :join? join?}))

(defn -main [& args]
  (start-server true))
