(ns imgsvr.handler
  ^{:author sean}
  (:use [imgsvr.config :as cfg]
        [image-resizer.core :only [resize-to-width]]
        [clojure.string :as str]
        [ring.util.response :only [response]]
        [image-resizer.util :only [buffered-image dimensions]]
        [image-resizer.core :only [resize-to-width]]
        )
  (:import (java.util UUID Date)
           (java.text SimpleDateFormat)
           (javax.imageio ImageIO)
           (java.io File)
           (java.awt.image BufferedImage)
           (java.lang Integer)
           (java.nio.file Files Paths Path LinkOption)
           (java.nio.file.attribute FileAttribute)))

(def nil-str-array (into-array String []))
(def nil-fileAttr-array (into-array FileAttribute []))
(def nil-linkOption-array (into-array LinkOption []))


(defn resize-img [^BufferedImage f width]
  "缩放图片"
  (if (integer? width)
    (resize-to-width f width)
    (resize-to-width f (Integer/parseInt width))))


(defn save-img-fs [^BufferedImage f ^String fmt ^File file]
  "写入文件"
  (ImageIO/write f fmt file))

(defn to-path [p]
  (if (string? p) (Paths/get p nil-str-array) p)
  )

(defn make-dir [path]
  "创建目录"
  (when-let [p (to-path path)]
    (if (not (Files/exists p nil-linkOption-array))
      (Files/createDirectories p nil-fileAttr-array)
      p)))


(defn subPath [save-path]
  "获取相对路径"
  (let [p (to-path save-path) abs-path (to-path (cfg/get-img-cfg-field "img.save.dir")) p-count (.getNameCount p)]
      (str "/" (.subpath p (- p-count (- p-count (.getNameCount abs-path))) p-count))
    ))


(defn save-img [^BufferedImage f userType userId mini-type]
  "处理文件名，目录"
  (let [size (dimensions f)
        img-fmt mini-type
        save-dir (str/join File/separator [(cfg/get-img-cfg-field "img.save.dir")
                                           (cfg/get-img-cfg-field "img.relative")
                                           userType
                                           userId
                                           (.format (SimpleDateFormat. "yyyy-MM-dd") (Date.))])
        file-name (str/join "." [(str (UUID/randomUUID) "_" (get size 0) "x" (get size 1)) img-fmt])
        save-file (str/join File/separator [save-dir file-name])]
    (do (make-dir save-dir) (save-img-fs f img-fmt (File. save-file))
        (subPath save-file)  )
    ))

(defn process-img [userType userId width f]
  (let [^BufferedImage bi (buffered-image (:tempfile f)) s (dimensions bi) w (Integer/parseInt width) iw (get s 0)]
    (if (>= iw w)
      (save-img
        (resize-img bi w)
        userType userId (get (str/split (:content-type f) #"/") 1)
        ))))

(defn proc-result [s]
  (if (seq? s)
    (if (empty? (filter (fn [se] (not (nil? se))) s))
      {:code  "100" :msg "上传失败"}
      {:code "0" :msg "ok" :data s}
      )
    ))


(defn upload [request]
  "程序入口"
  (let [{:keys [userType userId width]} (:params request)
        files ((:multipart-params request) "file")]
    (let [process (partial process-img userType userId width)]
      (response (proc-result (map process (filter (fn [f] (.contains (:content-type f) "image")) files)))))))
