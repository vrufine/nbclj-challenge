(ns web.domains.api
  (:require [org.httpkit.server :as s]
            [compojure.core :refer :all]
            [ring.middleware.json :as middleware]
            [web.domains.game :as game]
            [web.domains.robots :as r]))

(defn app []
  (routes
    (GET "/" [:as req]
      {:status 200
       :headers {"Content-Type" "application/json"}
       :body {:online true}})
    (GET "/game/grid" [:as req]
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (get (game/get-current-game) 'html)})
    (POST "/game/new" [:as req]
      (do
        (game/new-game)
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body (get (game/get-current-game) 'data)}))
    (POST "/robot/create" request
      (let [x (get-in request [:body :x])
            y (get-in request [:body :y])
            facing (get-in request [:body :facing])
            new-robot (r/create-robot x y facing)
            curr-game (game/place-robot (get (game/get-current-game) 'ref) new-robot)]
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body (get (game/get-current-game) 'data)}))))

(defn -main []
  (do
    (println "Server on http://localhost:8080/")
    (s/run-server
      (-> (app)
          (middleware/wrap-json-body {:keywords? true})
          (middleware/wrap-json-response))
      {:port 8080})))