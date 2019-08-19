(ns web.core
  (:require [org.httpkit.server :as s]
            [compojure.core :refer :all]))

(defonce ^:private server (atom nil))

(defn remove-trailing-backslash [handler]
  (fn [req]
    (let [uri (:uri req)
          not-root? (not= uri "/")
          ends-with-slash? (.endsWith ^String uri "/")
          fixed-uri (if (and not-root?
                             ends-with-slash?)
                      (subs uri 0 (dec (count uri)))
                      uri)
          fixed-req (assoc req :uri fixed-uri)]
      (handler fixed-req))))

(defn app []
  (routes
    (GET "/" [:as req]
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body "<h1>Hello from Root \"/\"</h1>"})
    (GET "/:user-name" [user-name :as req]
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (format "<h1>Hello, %s </h1>" user-name)})))

(defn create-server []
  (s/run-server (remove-trailing-backslash (app)) {:port 8080}))

(defn stop-server [server]
  (server :timeout 100))
