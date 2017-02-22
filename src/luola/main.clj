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

(def turn-duration 200000) ;; in ms


(defn uuid []
  (str (java.util.UUID/randomUUID)))

(defn timestamp []
   (.getTime (java.util.Date.)))

(def PlayerAction (s/enum "move" "attack"))

(def ActionTarget (s/enum "north" "east" "south" "west"))

(defn make-action [action-type target]
  {:type action-type
   :target target})

(defn set-action-proposal! [actor action]
   (alter-world!
      (fn [turn board moves]
         [turn board
            (assoc moves (:name actor)
               {:action action
                :timestamp (timestamp)})])))


;;; World time

(defn time-ticker []
   (loop []
      (println "Turn " (turn) "ends.")
      (alter-world!
         (fn [turn board moves]
            [(+ turn 1) board {}]))
      (Thread/sleep turn-duration)
      (recur)))

(defonce game-time
   (let [thread (Thread. time-ticker)]
      (.start thread)
      thread))


;;; Game state

;; set value to just [val]
(defn put-board [board x y val]
   (assoc board y
      (assoc (get board y {}) x [val])))

(defn get-board [board x y def]
   (get (get board y {}) x def))

(def empty-cell
   {:type :ground
    :value :empty})

(def wall-cell
   {:type :thing
    :value :wall})

;; todo: add ip
(defn make-player [name pass]
   {:type :player
    :name name
    :pass pass})

(defn parse-board [string]
   (loop [board {} x 0 y 0 data (seq string)]
      (cond
         (empty? data)
            board
         (= (first data) \newline)
            (recur board 0 (+ y 1) (rest data))
         (= (first data) \.)
            (recur
               (put-board board x y empty-cell)
               (+ x 1) y (rest data))
         (= (first data) \space)
            (recur board x y (rest data))
         (= (first data) \#)
            (recur (put-board board x y wall-cell) (+ x 1) y (rest data))
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


(defn player? [thingie]
  (and (map? thingie)
       (= (:type thingie) :player)))

(defn find-player [board name]
  (reduce (fn [state [y xs]]
            (reduce (fn [state [x vals]]
               (reduce (fn [state val]
                      (or state
                          (if (and (player? val)
                                   (= (:name val) name))
                            val)))
                      state vals))
                    state
                    xs))
          nil
          board))

(defn find-authorized-player [board name pass]
  (let [player (find-player board name)]
    (when (= (:pass player) pass)
      player)))

(defn maybe-add-player! [name pass]
   (alter-world!
      (fn [turn board actions]
         (if (find-player board name)
            [turn board actions]
            (let [pos (empty-pos board)]
              (if pos
                  [turn 
                     (put-board board (nth pos 0) (nth pos 1) 
                        (make-player name pass))
                     actions]
                  [turn board actions]))))))

(defn reset-game! []
   (alter-world!
      (fn [_ _ _]
         [1
            (parse-board  "###############################
                           #...............#.............#
                           #...............#.............#
                           #.............................#
                           #...............#.............#
                           #...............#.............#
                           #...............########.######
                           #...............#.............#
                           #...............#.............#
                           #...............#.............#
                           ########################.######
                           #.............................#
                           #.............................#
                           #.............................#
                           #.............................#
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
               (do (set-action-proposal! player (make-action action target))
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
