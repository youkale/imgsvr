(ns
  ^{:author sean}
  imgsvr.route
  (:use [ring.util.response :only [response resource-response url-response]]
        [compojure.core :only [GET POST PUT DELETE defroutes]]
        [compojure.route :only [not-found resources files]]
        [ring.middleware.multipart-params :as mp]
        [imgsvr.config :as cfg ]
        [imgsvr.handler :as handler]
        ))



(defroutes app-routers
           (mp/wrap-multipart-params (POST "/api/v1/upload/:userType/:userId/:width" [] handler/upload))
           (GET "/" [] {:body {:template "index.html" :model {}}} )
           (files "/" {:root (cfg/get-img-cfg-field "img.save.dir")} )
           (not-found "<h1>sorry!!! 404 </h1>"))
