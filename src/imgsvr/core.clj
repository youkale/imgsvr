(ns imgsvr.core
  ^{:author sean}
  (:use [ring.adapter.jetty :as jetty]
        [ring.middleware.keyword-params :as keyword-params]
        [ring.middleware.cookies :as cookies]
        [ring.middleware.params :as params]
        [ring.middleware.json :as json]
        [imgsvr.route :as route]
        [imgsvr.layout :as layout]
        [imgsvr.warp :as wildcoder]
        )
  (:gen-class)
  )

(defn init []
  (println "imageserver is starting"))

(defn destroy []
  (println "imageserver is shutting down"))



(def app (-> route/app-routers
             cookies/wrap-cookies
             keyword-params/wrap-keyword-params ;字符串参数转关键字
             params/wrap-params  ;query-string 转参数了
             layout/wrap-template-response  ;转换模板
             json/wrap-json-response            ;; render json
             json/wrap-json-body                ;; request json
             wildcoder/warp-svr ;转换Response head Server
             )
  )

;; (defn start-server []
;;   (jetty/run-jetty app {:host "0.0.0.0",
;;                   :port 3000
;;                   }
;;              )
;;   )

;; (defn -main [& args]
;;   (if (= "start" (first args))
;;     (start-server)))
