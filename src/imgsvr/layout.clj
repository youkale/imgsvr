(ns
  ^{:author sean}
  imgsvr.layout
  (:use [ring.util.response :as response]
        [selmer.parser :as parser]
        )
  )

(parser/cache-off!)
(parser/set-resource-path! (clojure.java.io/resource "public"))

(defn try-render [body]
  (if (map? body)
    (let [[model template] [(:model body) (:template body)]]
      (if (and (map? model) (string? template))
        (parser/render-file template model)))))

(defn take-need-temp [[_ v]]
  (and (map? v)
       (contains? v :model)
       (contains? v :template)))

(defn eval-sub-temp [[k v]]
  {k (try-render v)})

(defn- iter-render [body]
  "if has sub template then eval it"
  (if-let [model (:model body)]
    (if-let [need-render (filter take-need-temp model)]
      (try-render (assoc body :model (merge model (apply merge (map eval-sub-temp need-render)))))
      (try-render body))))


(defn wrap-template-response
  "Middleware that converts responses with a map for a body into a
  templating string response."
  [handler]
  (fn [request]
    (if-let [resp (handler request)]
      (if-let [render-result (iter-render (:body resp))]
          (let [templet-response (assoc resp :body render-result)]
            (if (contains? (:headers resp) "Content-Type")
              templet-response
              (response/content-type templet-response "text/html; charset=utf-8"))
            )
          resp))))