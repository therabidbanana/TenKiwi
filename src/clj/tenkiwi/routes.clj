(ns tenkiwi.routes
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [compojure.core :refer [ANY GET PUT POST DELETE routes]]
            [ring.middleware.anti-forgery :as csrf]
            [compojure.route :refer [resources]]
            [ring.util.response :refer [response]]))

(defn home-routes [endpoint]
  (routes
   (GET "/" _
     (-> "public/index.html"
         io/resource
         io/input-stream
         response
         (update-in [:body] #(str/replace (slurp %) "CSRF_TOKEN" (force csrf/*anti-forgery-token*)))
         (assoc :headers {"Content-Type" "text/html; charset=utf-8"})))
   (GET "/cards.html" _
        (-> "public/cards.html"
            io/resource
            io/input-stream
            response
            (update-in [:body] #(str/replace (slurp %) "CSRF_TOKEN" (force csrf/*anti-forgery-token*)))
            (assoc :headers {"Content-Type" "text/html; charset=utf-8"})))
   (GET "/.well-known/apple-app-site-association" _
        (-> "public/.well-known/apple-app-site-association"
            io/resource
            io/input-stream
            response))
   (GET "/.well-known/assetlinks.json" _
        (-> "public/.well-known/assetlinks.json"
            io/resource
            io/input-stream
            response))
   (resources "/")))
