(defproject luola "0.0.0-SNAPSHOT"
   :dependencies 
      [[org.clojure/clojure "1.8.0"]
       [prismatic/schema "1.1.3"]
       [prismatic/plumbing "0.5.3"]
       ;[metosin/potpuri "0.4.0"]
       [ring/ring "1.5.0"]
       [ring/ring-defaults "0.2.1"]
       [metosin/compojure-api "1.2.0-alpha2"]
       [metosin/ring-http-response "0.8.0"]
       [metosin/ring-swagger "0.22.14"]
       [metosin/ring-swagger-ui "2.2.5-0"]
       [clj-http "3.4.1"]
       ;[org.clojure/core.async "0.2.395"]
       ;[prismatic/dommy "1.1.0"]
       ;[org.clojure/tools.cli "0.3.1"]
       ]

   :source-paths ["src"]

   :profiles 
      {:dev 
         {:dependencies  []
            ; [[ring-mock "0.1.5"]]
          :resource-paths ["target/generated"]}
       :uberjar 
          {:main  luola.main
           :aot   [luola.main]
           :uberjar-name "luola.jar"}}
           
   :repl-options 
      {:init-ns luola.main
       :init (do (println "Repl init") (go))
       :timeout 900000 ; 120s, needed for slow machines
       }
       
   :main luola.main)
   
