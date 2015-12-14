(defproject imgsvr "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/data.json "0.2.6"]
                 [image-resizer "0.1.8"]
                 [selmer "0.8.9"]
                 [ring "1.4.0"]
                 [ring-server "0.3.1"]
                 [compojure "1.4.0"]
                 [ring/ring-json "0.3.1"]
                 [org.clojure/tools.logging "0.3.1"]
                 ]
  :plugins [[lein-ring "0.9.7"]]

  :ring {:handler imgsvr.core/app
         :init imgsvr.core/init
         :destroy imgsvr.core/destroy}

  :profiles
  {:uberjar {:aot :all}
   :production
            {:ring
             {:open-browser? false, :stacktraces? false, :auto-reload? false}}
   :dev
            {:dependencies [[ring-mock "0.1.5"] [ring/ring-devel "1.3.1"]]}}

  )
