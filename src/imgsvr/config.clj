(ns imgsvr.config
  (:use [clojure.java.io :as io])
  (:import (java.util Properties)))


(defn read-cfg [path]
  "读取配置文件工具类"
  (with-open [r (-> path io/resource io/reader)]
    (let [p (Properties.)]
      (do (.load p r) p))))

(def img-cfg (delay (read-cfg "image.properties")))

(defn get-img-cfg-field [field]
    (get @img-cfg field)
  )
