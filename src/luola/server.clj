(ns luola.server
  (:require [compojure.api.sweet :refer :all]
            [ring.util.http-response :refer [ok status content-type] :as resp]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.adapter.jetty :as jetty]
            [ring.swagger.upload :as upload]
            [schema.core :as s]
            [taoensso.timbre.appenders.core :as appenders]
            [clojure.java.shell :refer [sh]]
            [pantomime.mime :refer [mime-type-of]]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]))

;;; Game state

(defonce board (atom {}))

(defn timestamp []
   (.getTime (java.util.Date.)))

(defn put-board [board x y val]
   (assoc board y
      (assoc (get board y {}) x val)))

(defn get-board [board x y def]
   (get (get board y {}) x def))

(defn parse-board [string]
   (loop [board {} x 0 y 0 data (seq string)]
      (cond 
         (empty? data)
            board
         (= (first data) \newline)
            (recur board 0 (+ y 1) (rest data))
         (= (first data) \.)
            (recur 
               (put-board board x y :empty)
               (+ x 1) y (rest data))
         (= (first data) \space)
            (recur board x y (rest data))
         (= (first data) \#)
            (recur (put-board board x y :wall) (+ x 1) y (rest data))
         :else
            (do
               (println "BAD CHAR:" (first data))
               (recur board x y (rest data))))))

(defn empty-pos [board]
   (reduce
      (fn [taken y]
         (reduce 
            (fn [taken x]
               (let [val (get-board board x y false)]
                  (if (= val :empty)
                     [x y]
                     taken)))
            taken (keys (get board y))))
      false
      (keys board)))

(defn make-player [name pass]
   {:type :player
    :name name
    :pass pass})
             
(defn add-player! [name pass]
   (swap! board
      (fn [board]
         (let [pos (empty-pos board)]
            (if pos
               (put-board board (nth pos 0) (nth pos 1) (make-player name pass))
               board)))))

(defn reset-game! []
   (reset! board
      (parse-board  "#######################
                     #......................#
                     #.......................#
                     #........................#
                     #######...................#
                     #.....#....................#
                     #...........................#
                     #.....#......................#
                     #.....#.....................#
                     #.....#....................# 
                     ###########################")))

(defn unparse-board [board player]
   (loop [x 0 y 0 out []]
      (let [val (get-board board x y false)]
         (cond
            (not val)
               (if (= x 0)
                  (apply str (reverse out))
                  (recur 0 (+ y 1) (cons \newline out)))
            (= val :wall)
               (recur (+ x 1) y (cons \# out))
            (= val :empty)
               (recur (+ x 1) y (cons \. out))
            (= :player (:type val))
               (if (= player (:name val))
                  (recur (+ x 1) y (cons \@ out))
                  (recur (+ x 1) y (cons \P out)))
            :else
               (recur (+ x 1) y (cons \? out))))))
               
;;; Handler

(def api-handler
   (api
      {:swagger
         {:ui "/api-docs"
          :spec "/swagger.json"
          :data {:info {:title "Taistelon API"
                        :description "keinot taisteluun"}}}}

      (undocumented
        (GET "/" []
          (resp/temporary-redirect "/index.html")))

      (GET "/pic/:type" []
         :summary "get player pic" 
         (ok "foo"))

      (context "/api" []

         (GET "/alive" []
            :summary "check whether server is running"
            (ok "I feel happy!"))
         
         (GET "/reset" []
            :summary "reset game"
            (ok (reset-game!)))
         
         (GET "/board" []
            :query-params [name :- s/Str]
            (ok (unparse-board @board name)))
               
         (GET "/add-player" []
            :query-params [name :- s/Str, pass :- s/Str]
            (let [me (add-player! name pass)]
               (ok me))))))


;;; Startup and shutdown

(defonce server (atom nil))

(defn stop-server []
   (when-let [s @server]
      (println "stopping server")
      (.stop s)
      (reset! server nil)))

(defn start-luola [handler conf]
   (when server
      (stop-server))
   (stop-server)
   (reset! server (jetty/run-jetty api-handler conf))
   (when server
     (println "Taistelo is running")))

(s/defn start-server [conf]
   (start-luola api-handler
      {:port 8080
       :join? false}))


;;; Dev mode entry

(defn reset [] 
   (if server
      (do
         (println "Stopping luola")
         (stop-server)))
   (require 'luola.server :reload)
   (start-server
      {:port 8080}))

(defn go []
   (println "reset from go")
   (reset))

