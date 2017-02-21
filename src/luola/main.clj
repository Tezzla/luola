(ns luola.main
   (:gen-class))

(defn -main [& args]
   (require 'luola.server)
   (let [start-server (resolve 'luola.server/start-server)]
      (start-server {})))
