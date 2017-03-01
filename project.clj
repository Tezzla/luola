(defproject luola "0.0.1"
   :dependencies
      [[org.clojure/clojure "1.8.0"]
       [prismatic/schema "1.1.3"]
       [prismatic/plumbing "0.5.3"]
       [ring/ring "1.5.0"]
       [ring/ring-defaults "0.2.1"]
       [metosin/compojure-api "1.2.0-alpha2"]
       [metosin/ring-http-response "0.8.0"]
       [metosin/ring-swagger "0.22.14"]
       [metosin/ring-swagger-ui "2.2.5-0"]
       [clj-http "3.4.1"]]

   :source-paths ["src"]
   :resource-paths ["resources"]

   :profiles
      {:dev
         {:dependencies  []
          :resource-paths ["target/generated"]}
       :uberjar
          {:main  luola.main
           :aot   [luola.main]
           :uberjar-name "luola.jar"}}

   :repl-options
      {:init-ns luola.main
       :init (do (println "Repl init") (go))
       :timeout 1000000
       }

   :main luola.main)
