(ns imgsvr.warp
  (:use [ring.util.response :as response])
  )

(defn warp-svr [handler]
  "将服务器信息覆盖成apache"
  (fn [request]
    (let [resp (handler request)]
      (response/header resp "Server" "nginx 3.1415926"))))
