(ns luola.main
  (:require [compojure.api.sweet :refer :all]
            [ring.util.http-response :refer [ok status content-type] :as resp]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.adapter.jetty :as jetty]
            [schema.core :as s]
            ;[clojure.string :as string]
            ;[cheshire.core :as json]
            )
   (:import java.util.UUID)
   (:gen-class))

(defonce world
   (atom [1 {} {}]))

(defn turn [] (nth @world 0))
(defn board [] (nth @world 1))

(defn alter-world! [op]
   (swap! world
      (fn [[turn board moves]]
         (op turn board moves))))

(def turn-duration 200) ;; in ms


(defn uuid []
  (str (java.util.UUID/randomUUID)))

(defn timestamp []
   (.getTime (java.util.Date.)))

(def PlayerAction (s/enum "move" "attack"))

(def ActionTarget (s/enum "north" "east" "south" "west"))

(defn make-action [player action-type target]
  {:type action-type
   :target target
   :name (:name player)
   :timestamp (timestamp)})

(defn set-action-proposal! [actor action]
   (alter-world!
      (fn [turn board moves]
         [turn board (assoc moves (:name actor) action)])))

(defn player? [thingie]
  (and (map? thingie)
       (= (:type thingie) :player)))

(defn fold-named [op state board]
   (reduce 
      (fn [state [y xs]]
         (reduce 
            (fn [state [x vals]]
               (if (:name (first vals))
                  (op state x y (first vals))
                  state))
            state xs))
      nil board))
    
;; board name -> {:x x :y y :player player-map}
(defn find-player [board name]
   (fold-named
      (fn [state x y val]
         (if (= (:name val) name)
            {:x x :y y :player val}
            state))
      nil board))

(defn get-board [board x y def]
   (get (get board y {}) x def))

(defn put-board [board x y val]
   (assoc board y
      (assoc (get board y {}) x val)))

(def empty-cell
   {:type :ground
    :value :empty})

(defn can-move? [board x y]
   (= (get-board board x y false) [empty-cell]))

(defn step [x y dir]
   (cond
      (= dir "north") [x (- y 1)]
      (= dir "south") [x (+ y 1)]
      (= dir "west") [(- x 1) y]
      (= dir "east") [(+ x 1) y]
      :else [x y]))

(defn maybe-move [board info dir]
   (let [x (:x info) y (:y info) 
         old (get-board board x y [])
         [xp yp] (step x y dir)]
      (if (can-move? board xp yp)
         (let [new (get-board board xp yp [])]
            (-> board
               (put-board x y (rest old))
               (put-board xp yp (cons (first old) new))))
         board)))
      
(defn step-world [board actions]
   (let [actions (sort (fn [a b] (< (:timestamp a) (:timestamp b))) (vals actions))]
      (reduce
         (fn [board action]
            ;(println "ACTION " action)
            (let [info (find-player board (:name action))]
               (cond
                  (= (:type action) "move")
                     (maybe-move board info (:target action))
                  :else
                     (do
                        (println "Unknown action: " action)
                        board))))
         board actions)))

;;; World time

(defn monster-actions []
   (fold-named
      (fn [state x y val]
         (if (= :monster (:type val))
            (set-action-proposal! val (make-action val "move" (rand-nth ["north" "south" "east" "west"])))
            state))
      nil (board)))

(defn time-ticker []
   (loop []
      ;(println "Turn " (turn) "ends.")
      (alter-world!
         (fn [turn board moves]
            [(+ turn 1) 
               (step-world board moves)
               {}]))
      (monster-actions)
      (Thread/sleep turn-duration)
      (recur)))

(defonce game-time
   (let [thread (Thread. time-ticker)]
      (.start thread)
      thread))


;;; Game state

(def wall-cell
   {:type :thing
    :value :wall})

(defn make-player [name pass]
   {:type :player
    :name name
    :pass pass})

(defn make-monster [name]
   {:type :monster
    :name name})
 
(defn parse-board [string]
   (loop [board {} x 0 y 0 data (seq string)]
      (cond
         (empty? data)
            board
         (= (first data) \newline)
            (recur board 0 (+ y 1) (rest data))
         (= (first data) \.)
            (recur
               (put-board board x y [empty-cell])
               (+ x 1) y (rest data))
         (= (first data) \space)
            (recur board x y (rest data))
         (= (first data) \#)
            (recur (put-board board x y [wall-cell]) (+ x 1) y (rest data))
         :else
            (do
               (println "BAD CHAR:" (first data))
               (recur board x y (rest data))))))

;; find an empty position
(defn empty-pos [board]
   (reduce
      (fn [taken y]
         (reduce
            (fn [taken x]
               (let [val (get-board board x y false)]
                  (if (= val [empty-cell])
                     [x y]
                     taken)))
            taken (keys (get board y))))
      false
      (keys board)))



;; board name pass → nil | player-map
(defn find-authorized-player [board name pass]
   (let [info (find-player board name)]
      (when (= (:pass (:player info)) pass)
         (:player info))))

(defn maybe-add-thing! [name thing]
   (alter-world!
      (fn [turn board actions]
         (if (find-player board name)
            [turn board actions]
            (let [pos (empty-pos board)]
              (if pos
                  [turn 
                     (put-board board (nth pos 0) (nth pos 1) 
                        (cons thing
                           (get-board board (nth pos 0) (nth pos 1) [])))
                     actions]
                  [turn board actions]))))))

(defn maybe-add-player! [name pass]
   (maybe-add-thing! name
      (make-player name pass)))

(defn maybe-add-monster! []
   (let [name (uuid)]
      (maybe-add-thing! name
         (make-monster name))))

(defn reset-game! []
   (alter-world!
      (fn [_ _ _]
         [1
            (parse-board  "#################################
                           #...............#...............#
                           #...............#.............#.#
                           #.............................#.#
                           #...............#.............#.######
                           #...............#.............#......#
                           #...............########.###########.#
                           #...............#..................#.#
                           #...............#..................#.#
                           #...............#..................#.#
                           ########################.###########.#
                           #....................................#
                           #...........................##########
                           #...........................#  
                           #...........................###
                           #.............................#
                           #.............................#
                           #.............................#
                           ###############################")
            {}])))

(defn unparse-board [board player]
   (loop [x 0 y 0 out []]
      (let [val (get-board board x y false)]
         (cond
            (not val)
               (if (= x 0)
                  (apply str (reverse out))
                  (recur 0 (+ y 1) (cons \newline out)))
            (= val [wall-cell])
               (recur (+ x 1) y (cons \# out))
            (= val [empty-cell])
               (recur (+ x 1) y (cons \. out))
            (= :player (:type (first val)))
               (if (= player (:name (first val)))
                  (recur (+ x 1) y (cons \@ out))
                  (recur (+ x 1) y (cons \P out)))
            (= :monster (:type (first val)))
               (recur (+ x 1) y (cons \$ out))
            :else
               (recur (+ x 1) y (cons \? out))))))


;;; Handler

(defn status-no-player []
  (status (ok "no") 404))

(def api-handler
   (api
      {:swagger
         {:ui "/api-docs"
          :spec "/swagger.json"
          :data {:info {:title "Luolan API"
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
            (do (reset-game!)
                (ok (unparse-board (board) nil))))

         (GET "/board" []
            :query-params [name :- s/Str]
            (ok (unparse-board (board) name)))

         (GET "/add-player" []
            :query-params [name :- s/Str, pass :- s/Str]
            (do
               (maybe-add-player! name pass)
               (if-let [player (find-authorized-player (board) name pass)]
                 (ok (unparse-board (board) (:name player)))
                 (status (ok "unacceptable") 403))))

         (GET "/player" []
           :query-params [name :- s/Str, pass :- s/Str]
           (let [player (find-authorized-player (board) name pass)]
             (if player
               (ok player)
               (status-no-player))))

         (GET "/act" []
           :query-params [name :- s/Str, pass :- s/Str, action :- PlayerAction, {target :- ActionTarget nil}]
           (let [player (find-authorized-player (board) name pass)]
             (if player
               (do (set-action-proposal! player (make-action player action target))
                   (ok))
               (status-no-player)))))))


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
     (println "Luola is running")))

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
   (require 'luola.main :reload)
   (reset-game!)
   (start-server
      {:port 8080}))

(defn go []
   (println "reset from go")
   (reset))

(defn -main [& args]
   (start-server {}))
