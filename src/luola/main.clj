(ns luola.main
  (:require [clojure.java.io :as io]
            [compojure.api.sweet :refer :all]
            [compojure.route :as route]
            [ring.util.http-response :refer [ok status content-type] :as resp]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.adapter.jetty :as jetty]
            [schema.core :as s])
   (:import java.util.UUID)
   (:gen-class))

(defn random-element [collection]
  (when (first collection)
    (rand-nth collection)))

(defonce next-board (atom (promise)))
(defonce world
  (atom [1  ; turn counter
         {} ; board
         {} ; moves
         [] ; limbo for dead players
         ]))

(defn turn [] (nth @world 0))
(defn board [] (nth @world 1))
(defn limbo [] (nth @world 3))

(defn next-board-state-promise [] @next-board)

(defonce master-password
   (atom
      (try (slurp "/tmp/luola-salasana.txt")
           (catch Exception e "salasana"))))

(defn alter-world! [op]
   (swap! world
      (fn [[turn board moves limbo]]
        (op turn board moves limbo))))

(defn deliver-next-board-state! [[_ new-board _ _]]
  (let [old-promise (next-board-state-promise)]
    (reset! next-board (promise))
    (deliver old-promise new-board)))

(defonce turn-duration (atom 412)) ;; in ms

(defn set-game-speed! [ms]
   (if (and (> ms 50) (< ms 30000))
      (do
         (reset! turn-duration ms)
         true)
      false))

(defn uuid []
  (str (java.util.UUID/randomUUID)))

(defn timestamp []
   (System/currentTimeMillis))

; Game objects

(def empty-cell
   {:type :ground
    :value :empty})

(defn empty-cell? [thingie]
  (= thingie empty-cell))

(def wall-cell
   {:type :thing
    :value :wall})

(defn wall-cell? [thingie]
  (= thingie wall-cell))

(def player-spawn-cell
  {:type :spawn-cell
   :value :player-spawn-cell})

(defn player-spawn-cell? [thingie]
  (= thingie player-spawn-cell))

(def item-spawn-cell
  {:type :spawn-cell
   :value :item-spawn-cell})

(defn item-spawn-cell? [thingie]
  (= thingie item-spawn-cell))

(defn spawn-cell? [thingie]
  (and (map? thingie)
       (= (:type thingie) :spawn-cell)))

(defn make-player [name pass]
  {:type  :player
   :items []
   :name  name
   :pass  pass})

(defn player? [thingie]
  (and (map? thingie)
       (= (:type thingie) :player)))

(defn make-monster [name]
   {:type :monster
    :name name})

(defn monster? [thingie]
  (and (map? thingie)
       (= (:type thingie) :monster)))

(defn make-item [name value]
  {:type :item
   :name name
   :value value})

(defn item? [thingie]
  (and (map? thingie)
       (= (:type thingie) :item)))

; Gather board cells

(defn get-board [board x y def]
  (get (get board y {}) x def))

(defn board-cells [board predicate]
  (loop [x 0 y 0 out []]
    (let [val (get-board board x y false)]
      (cond
        (not val)
          (if (= x 0)
            out
            (recur 0 (+ y 1) out))
        (predicate val)
          (recur (+ x 1) y (cons {:x x :y y :cell val} out))
        :else
          (recur (+ x 1) y out)))))

(defn player-cells [board]
  (board-cells board #(player? (first %))))

(defn player-spawn-cells [board]
  (board-cells board #(player-spawn-cell? (first %))))

(defn empty-player-spawn-cell [board]
  (random-element (player-spawn-cells board)))

(defn item-spawn-cells [board]
  (board-cells board #(item-spawn-cell? (first %))))

(defn empty-item-spawn-cell [board]
  (random-element (item-spawn-cells board)))

; Actions

(def PlayerAction (s/enum "move" "attack"))

(def ActionTarget (s/enum "north" "east" "south" "west"))

(defn make-action [player action-type target]
  {:type action-type
   :target target
   :name (:name player)
   :timestamp (timestamp)})

(defn set-action-proposal! [actor action]
   (alter-world!
      (fn [turn board moves limbo]
         [turn board (assoc moves (:name actor) action) limbo])))

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
(defn find-named [board name]
   (fold-named
      (fn [state x y val]
         (if (= (:name val) name)
            {:x x :y y :player val}
            state))
      nil board))

(defn put-board [board x y val]
   (assoc board y
      (assoc (get board y {}) x val)))

; limbo

(defn ->limbo [thingie turns-in-limbo]
  (assoc thingie :turns-in-limbo turns-in-limbo))

(defn <-limbo [thingie]
  (dissoc thingie :turns-in-limbo))

(defn add-to-limbo [limbo thingie turns-in-limbo]
  (conj limbo (->limbo thingie turns-in-limbo)))

(defn advance-limbo-time [limbo]
  (map #(update % :turns-in-limbo dec) limbo))

(defn release-from-limbo [limbo]
  [(filter #(< (:turns-in-limbo %) 0) limbo)
   (remove #(< (:turns-in-limbo %) 0) limbo)])

(def player-turns-in-limbo 10)
(defn item-turns-in-limbo []
  (+ 100 (rand-int 100)))
; move

(defn can-move? [board x y]
  (let [val (get-board board x y [])]
    (or (item? (first val))
        (spawn-cell? (first val))
        (empty-cell? (first val)))))

(defn enemy-can-move? [board x y]
   (let [val (get-board board x y [])]
     (or (empty-cell? (first val))
         (monster? (first val))
         (item? (first val)))))

(defn step [x y dir]
   (cond
      (= dir "north") [x (- y 1)]
      (= dir "south") [x (+ y 1)]
      (= dir "west") [(- x 1) y]
      (= dir "east") [(+ x 1) y]
      :else [x y]))

(defn maybe-move
  "Move entity to given direction, if able. If entity is a player and
  there is an item in the room, the item is added to both limbo and the
  player's inventory"
  [board limbo {:keys [x y]} dir]
  (let [old (get-board board x y [])
        [xp yp] (step x y dir)]
    (if (can-move? board xp yp)
      (let [new (get-board board xp yp [])
            old-entity (first old)
            player-moving-on-item? (and (player? old-entity)
                                        (item? (first new)))
            new-entity (if player-moving-on-item?
                         (update old-entity :items conj (first new))
                         old-entity)
            new-limbo (if player-moving-on-item?
                        (add-to-limbo limbo (first new) (item-turns-in-limbo))
                        limbo)]
        [(-> board
             (put-board x y (rest old))
             (put-board xp yp (cons new-entity
                                    (if player-moving-on-item?
                                      (rest new)
                                      new))))
         new-limbo])
      [board limbo])))

; attack

(defn can-attack? [board x y]
  (let [val (get-board board x y [])]
    (player? (first val))))

(defn maybe-kill-target [board limbo x y]
  (if-let [[target & rest-of-cell] (get-board board x y [])]
    [(put-board board x y rest-of-cell)
     (add-to-limbo limbo target player-turns-in-limbo)]
    [board limbo]))

; board limbo target dir -> [board limbo]
(defn maybe-attack [board limbo {:keys [x y]} dir]
  (let [[xp yp] (step x y dir)]
    (if (can-attack? board xp yp)
      (maybe-kill-target board limbo xp yp)
      [board limbo])))

(defn resolve-actions [board actions limbo]
  (reduce
   (fn [[board limbo] action]
     ;(println "ACTION " action)
     (let [info (find-named board (:name action))]
       (cond
         (nil? info)
           [board limbo]
         (= (:type action) "move")
           (maybe-move board limbo info (:target action))
         (= (:type action) "attack")
           (maybe-attack board limbo info (:target action))
         :else
           (do
             (println "Unknown action: " action)
             [board limbo]))))
   [board limbo]
   actions))

(defn update-limbo [board limbo]
  ; take out the entities ready for release ...
  (let [[release-candidates new-limbo] (release-from-limbo (advance-limbo-time limbo))]
    ; ... only for them to be thrown back to limbo if there's no room in the mortal world
    (reduce (fn [[board limbo] candidate]
              (if-let [{:keys [x y]} (cond (player? candidate)
                                             (empty-player-spawn-cell board)
                                           (item? candidate)
                                             (empty-item-spawn-cell board)
                                           :else
                                             (println "inalid entity in limbo: " candidate))]
                 [(put-board board x y
                             (cons (<-limbo candidate)
                                   (get-board board x y [])))
                  limbo]
                 [board
                  (add-to-limbo limbo candidate (:time-in-limbo candidate))]))
            [board new-limbo]
            release-candidates)))

(defn step-world [board actions limbo]
   (let [actions (sort (fn [a b] (< (:timestamp a) (:timestamp b))) (vals actions))]
     (->> (resolve-actions board actions limbo)
          (apply update-limbo))))

;;; World time

(defn unvisited-neighbours [map board poss]
   (set
      (reduce
         (fn [out [x y]]
            (reduce
               (fn [out [x y]]
                  (cond
                     (get-board map x y false)
                        out
                     (enemy-can-move? board x y)
                        (cons [x y] out)
                     :else
                        out))
               out
               [[(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]]))
         [] poss)))

(defn player-distance-map
  "a scalar field showing the distance to the nearest player"
  []
   (let [board (board)
         roots
            (fold-named
               (fn [roots x y val] (if (player? val) (cons [x y] roots) roots))
               [] board)]
      (loop [poss roots distance 0 map {}]
         ;(println "unvisited-neighbours " poss)
         (if (empty? poss)
            map
            (let [map (reduce (fn [map [x y]] (put-board map x y distance)) map poss)]
               (recur
                  (unvisited-neighbours map board poss)
                  (+ distance 1)
                  map))))))

(defn jitter []
   (or
      (get [1 -1 1 -1 1 -1 1 -1 2 -2 8 -8 32 -32] (rand-int 100))
      0))

(defn bestish-direction [mmap x y]
   (let [opts
         (map
          (fn [[x y dir]] [(+ (jitter) (get-board mmap x y 1000)) x y dir])
            [[(+ x 1) y "east"]
             [(- x 1) y "west"]
             [x (+ y 1) "south"]
             [x (- y 1) "north"]])
         bestish (first (sort (fn [a b] (< (first a) (first b))) opts))]
      (if (= (nth bestish 0) 1000)
         (rand-nth opts)
         bestish)))



(defn monster-action [monster board player-distance-map x y]
  (let [[distance x y direction] (bestish-direction player-distance-map x y)
        cell (get-board board x y [])]
    (cond (player? (first cell)) (make-action monster "attack" direction)
          :else                  (make-action monster "move" direction))))

(defn monsters-think []
  (let [map (player-distance-map)
        the-board (board)]
      (fold-named
         (fn [state x y val]
            (if (monster? val)
               (if (= (bit-and (turn) 1) 0)
                  ;; move half of the time
                  (set-action-proposal! val
                                        (monster-action val the-board map x y))
                  state)
               state))
         nil the-board)))

(defn time-ticker []
   (loop []
      ;(println "Turn " (turn) "ends.")
     (-> (alter-world!
          (fn [turn board moves limbo]
            (let [[new-board new-limbo] (step-world board moves limbo)]
              [(+ turn 1)
               new-board
               {}
               new-limbo])))
         (deliver-next-board-state!))
      ;; todo, substract this from sleep
      (let [turn-start (timestamp)
            ignore (monsters-think)
            turn-end (+ turn-start @turn-duration)]
         (Thread/sleep (max 10 (- turn-end (timestamp))))
         (recur))))

(defonce game-time
   (let [thread (Thread. time-ticker)]
      (.start thread)
      thread))


;;; Game state

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
         (= (first data) \$)
            (recur (put-board board x y [(make-item (uuid) 10) item-spawn-cell]) (+ x 1) y (rest data))
         (= (first data) \e)
            (recur (put-board board x y [(make-monster (uuid)) empty-cell]) (+ x 1) y (rest data))
         (= (first data) \#)
            (recur (put-board board x y [wall-cell]) (+ x 1) y (rest data))
         (= (first data) \:)
            (recur (put-board board x y [player-spawn-cell]) (+ x 1) y (rest data))
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
                  (if (empty-cell? (first val))
                     [x y]
                     taken)))
            taken (keys (get board y))))
      false
      (keys board)))



;; board name pass → nil | player-map
(defn find-authorized-player [board name pass]
   (let [info (find-named board name)]
      (when (= (:pass (:player info)) pass)
         (:player info))))

(defn maybe-add-thing!
  ([name thing]
   (maybe-add-thing! name thing empty-pos (fn [board _ name] (find-named board name))))
  ([name thing pos-fn already-exists?]
   (alter-world!
    (fn [turn board actions limbo]
      (if (already-exists? board limbo name)
        [turn board actions]
        (let [[x y :as pos] (pos-fn board)]
          (if pos
            [turn
             (put-board board x y
                        (cons thing
                              (get-board board x y [])))
             actions]
            [turn board actions limbo])))))))

(defn maybe-add-player! [name pass]
  (maybe-add-thing! name
                    (make-player name pass)
                    #(if-let [pos (empty-player-spawn-cell %)]
                       [(:x pos) (:y pos)])
                    (fn [board limbo name]
                      (or ((->> limbo (map :name) (set)) name)
                          (find-named board name))))
   true)

(defn maybe-add-monster! []
   (let [name (uuid)]
      (maybe-add-thing! name
                        (make-monster name)))
   true)

(def levels
(list
"##########################
 #$...........:..........$#
 #......:...........:.....#
 #........................#
 #:...........$..........:#
 #........................#
 #......:...........:.....#
 #........................#
 #$...........:..........$#
 ##########################"

"###############################################
 #:...........................................:#
 #.............................................#
 #.............................................#
 #.............................................#
 #.............................................#
 #.............................................#
 #.....................e.e.e...................#
 #......................$$$....................#
 #.....................e$$$e...................#
 #......................$$$....................#
 #.....................e.e.e...................#
 #.............................................#
 #.............................................#
 #.............................................#
 #.............................................#
 #.............................................#
 #.............................................#
 #.............................................#
 #:...........................................:#
 ###############################################"
 
"#############################################################################################################################################################
 #::::::.....................................................................................................................................................#
 #::::::.................................................................................................#...................................................#
 #::::::.................................................................................................#...#########...#########...#########...#########...#
 #.......................................................................................................#...#.......#...#.......#...#.......#...#:::::::#...#
 #.......................................................................................................#...#...$...#...#...e...#...#...e...#...#:::::::#...#
 #.......................................................................................................#...#.......#...#.......#...#.......#...#:::::::#...#
 #.......................................................................................................#...#.......#...#.......#...#.......#...#:::::::#...#
 #.......................................................................................................#...####.####...####.####...####.####...####.####...#
 #.......................................................................................................#...................................................#
 #.......................................................................................................#...................................................#
 #.......................................................................................................#...#########...#########...#########...#########...#
 #.......................................................................................................#...#.......#...#.......#...#.......#...#.......#...#
 #....................................................$..................................................#...#...e...#...#...e...#...#...e...#...#...e...#...#
 #.......................................................................................................#...#.......#...#.......#...#.......#...#.......#...#
 #.......................................................................................................#...#.......#...#.......#...#.......#...#.......#...#
 #.......................................................................................................#...####.####...####.####...####.####...####.####...#
 #.......................................................................................................#...................................................#
 #.......................................................................................................#...................................................#
 #.......................................................................................................#...#########...#########...#########...#########...#
 #.......................................................................................................#...#:::::::#...#.......#...#.......#...#.......#...#
 #.......................................................................................................#...#:::::::#...#...e...#...#...e...#...#...$...#...#
 #.......................................................................................................#...#:::::::#...#.......#...#.......#...#.......#...#
 #.......................................................................................................#...#:::::::#...#.......#...#.......#...#.......#...#
 #.......................................................................................................#...####.####...####.####...####.####...####.####...#
 #.......................................................................................................#...................................................#
 #.......................................................................................................#...................................................#
 #####################################.#################################################################.###################################################.#
 #..................................................................................................$$$#.....................................................#
 #.#####################################################################################################.....................................................#
 #...........................................................................................................................................................#
 #...........................................................................................................................................................#
 #...........................................................................................................................................................#
 #...........................................................................................................................................................#
 #.......................................................................................................................................................#####
 #...............................................................................................................................................#######.#...#
 #.............................................................................................................................................##..........#.#
 #.............................................................................................................................................#############.#
 #.........................................................................................................................................#####.............#
 #.........................................................................................................................................#.....###########.#
 #.........................................................................................................................................#.#####...#.......#
 #.........................................................................................................................................#.#...#.#.#########
 #.........................................................................................................................................#.#.#.#.#.........#
 #.........................................................................................................................................#.#.#.#.#.#######.#
 #eee......................................................................................................................................#.#.#.#.#.#e$e$e$e#
 #$$e......................................................................................................................................#.#.#.#.#.#$e$e$e$#
 #$$e......................................................................................................................................#...#...#..e$e$e$e#
 #############################################################################################################################################################"))

(def maze-level (slurp (io/resource "maze.level")))

(defn reset-game! [n]
   (println "Resetting game to level" n)
   (deliver-next-board-state! [nil nil nil nil])
   (alter-world!
      (fn [_ _ _ _]
         [1 (parse-board (nth levels (max 0 (min n (count levels))))) {} []]))
   :ok)

(defn player-char [node name]
   (cond
      (= name "overview")
         (first (.toUpperCase (:name node)))
      (= name (:name node))
         \@
      :else
         \P))
   
(defn unparse-board [board player]
   (loop [x 0 y 0 out []]
      (let [val (get-board board x y false)]
         (cond
            (not val)
               (if (= x 0)
                  (apply str (reverse out))
                  (recur 0 (+ y 1) (cons \newline out)))
            (wall-cell? (first val))
               (recur (+ x 1) y (cons \# out))
            (empty-cell? (first val))
               (recur (+ x 1) y (cons \. out))
            (item-spawn-cell? (first val))
               (recur (+ x 1) y (cons \. out))
            (player-spawn-cell? (first val))
               (recur (+ x 1) y (cons \: out))
            (player? (first val))
               (recur (+ x 1) y (cons (player-char (first val) player) out))
            (monster? (first val))
               (recur (+ x 1) y (cons \e out))
            (item? (first val))
               (recur (+ x 1) y (cons \$ out))
            :else
               (recur (+ x 1) y (cons \? out))))))


;;; Leaderboard

(defn players [board]
  (map (comp first :cell) (player-cells board)))

(defn player->leaderboard-entry [player]
  {:name  (:name player)
   :value (->> player
               :items
               (map :value)
               (reduce +))})

(defn leaderboard [board]
  (sort-by :value > (map player->leaderboard-entry (players board))))

(defn safe-name [s]
   (clojure.string/replace s #"[^a-zA-ZäöÄÖ0-9_-]" "_"))

(defn leaderboard-html []
   (reduce
      (fn [top node]
         (str top (safe-name (:name node)) ": " (:value node) "<br>\n"))
      "" (leaderboard (board))))

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
         (route/resources "/")
         (GET "/" []
            (resp/temporary-redirect "/index.html")))

      (context "/api" []

         (GET "/alive" []
            :summary "check whether server is running"
            (ok "I feel happy!"))

         (GET "/reset" []
            :summary "reset game"
            :query-params [pass :- s/Str, level :- s/Int]
            (if (= pass @master-password)
               (do (reset-game! level)
                   (ok (unparse-board (board) nil)))
                (ok "no")))

         (GET "/turn-duration" []
            :summary "get turn duration in ms"
            (ok (str @turn-duration)))

         (PUT "/turn-duration" []
            :query-params [value :- s/Int, pass :- s/Str]
            (if (= pass @master-password)
               (do
                  (reset! turn-duration value)
                  (ok "done"))
               (ok "no")))

         (GET "/board" []
            :query-params [name :- s/Str]
            (ok (unparse-board (board) name)))

         (GET "/next-board" []
            :query-params [name :- s/Str]
            (ok (unparse-board @(next-board-state-promise) name)))

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
               (status-no-player))))
      
        (GET "/leaderboard" []
           (ok (leaderboard-html))))))


;;; Startup and shutdown

(defonce server (atom nil))

(defn stop-server []
   (when-let [s @server]
      (println "stopping server")
      (.stop s)
      (reset! server nil)))

(defn start-luola [handler conf]
   (when @server
      (stop-server))
   (println "starting server")
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
   (reset-game! 0)
   (start-server
      {:port 8080}))

(defn go []
   (println "Go!")
   (reset-game! 0)
   (start-server {:port 8080}))

(defn -main [& args]
   (start-server {}))
