(ns thats-life.core
  (:require [reagent.core :as reagent]))

(def roll
  #(inc (rand-int 6)))

(def init-trail
  (vec
    (concat
      (range -1 -9 -1)
      (repeat 6 0)
      (range -1 -11 -1))))

(def guard -1)
(def guard? (partial = guard))

(defn setup-pawns [trail]
  (mapv
    #(vec (remove nil? (conj %1 %2)))
    (repeat (count trail) nil)
    (concat
      (repeat 8 nil)
      (repeat 8 guard)
      (repeat nil))))

(def init
  {:players []})

(defn starting-pawns [player-count]
  (nth [3 3 2 2] (- player-count 2)))

(defn join [state name]
  (update state :players #(conj % name)))

(defn moved [state]
  (remove guard? (apply concat (get state :pawns))))

(defn unmoved [state]
  (get state :start))

(defn in-play [state]
  (concat
    (unmoved state)
    (moved state)))

(defn playing [state]
  (-> state in-play distinct sort))

(defn started? [state]
  (contains? state :start))

(defn over? [state]
  (empty? (in-play state)))

(defn up [state]
  (get state :up))

(defn remove-once [pred xs]
  (let [[ys zs] (split-with (complement pred) xs)]
    (concat ys (rest zs))))

(defn unconj [xs x]
  (vec (remove-once (partial = x) xs)))

(defn start [state]
  (let [player-count (count (get state :players))]
    (-> state
      (assoc :start (vec (mapcat #(repeat (starting-pawns player-count) %) (range player-count))))
      (assoc :trail init-trail)
      (assoc :pawns (setup-pawns init-trail))
      (assoc :collected (vec (repeat player-count [])))
      (assoc :up (rand-int player-count))
      (assoc :die (roll)))))

(defn score [collected]
  (let [pos   (filter pos?  collected)
        neg   (filter neg?  collected)
        lucky (filter zero? collected)]
    (reduce + 0
      (concat pos
        (map *
          (sort (filter neg? collected))
          (concat (repeat (count lucky) -1) (repeat 1)))))))

(defn scores [state]
  (mapv score (get state :collected)))

(defn standings [state]
  (->>
    (map vector (scores state) (get state :players))
    (sort-by first)
    reverse
    vec))

(defn next-up [state]
  (let [player  (get state :up)
        players (set (playing state))
        turns   (take 6 (filter players (cycle (range (count (get state :players))))))]
    (if (over? state)
      (-> state
        (assoc :standings (standings state))
        (dissoc :up :die))
      (-> state
        (assoc :up (second (concat (drop-while #(not= player %) turns) turns)))
        (assoc :die (roll))))))

(defn has? [x xs]
  (some #{x} xs))

(defn drop-at [pos xs]
  (let [[ys zs] (split-at pos xs)]
    (vec (concat ys (rest zs)))))

(defn collect [state from]
  (let [player (up state)]
    (if (empty? (get-in state [:pawns from]))
      (-> state
        (update-in [:collected player] #(conj % (nth (get state :trail) from)))
        (update :trail #(drop-at from %))
        (update :pawns #(drop-at from %)))
      state)))

(defn trim [state]
  (if (empty? (get state :start))
    (let [n (count (take-while empty? (map #(remove guard? %) (get state :pawns))))]
      (-> state
        (update :trail #(vec (drop n %)))
        (update :pawns #(vec (drop n %)))))
    state))

(defn embark [state]
  (let [player (up state)]
    (if ((set (unmoved state)) player)
      (-> state
        (update :start #(unconj % player))
        (update-in [:pawns (- (get state :die) 1)] #(conj % player))
        trim
        next-up)
      state)))

(defn take-pawn [state from pawn]
  (update-in state [:pawns from] #(unconj % pawn)))

(defn exited? [state to]
  (not (contains? (get state :trail) to)))

(defn put-pawn [state to pawn]
  (if (exited? state to)
    state
    (update-in state [:pawns to] #(conj % pawn))))

(defn move-pawn [state from to pawn]
  (-> state
    (take-pawn from pawn)
    (put-pawn to pawn)))

(defn continue [state from pawn]
  (let [player (up state)
        pawns  (get-in state [:pawns from])
        to     (+ from (get state :die))]
    (if (and (has? player pawns) (or (= player pawn) (guard? pawn)))
      (-> state
        (move-pawn from to pawn)
        (collect from)
        trim
        next-up)
      state)))

(defn moves
  ([state pawn]
    (filter #(= pawn (second %)) (moves state)))
  ([state]
    (let [player (up state)]
      (distinct
        (concat
          (->>
            (get state :start)
            (filter (partial = player))
            (map (partial vector -1)))
          (->>
            (get state :pawns)
            (map-indexed
              (fn [idx pawns]
                (map
                  #(vector idx %)
                  (filter
                    (fn [pawn]
                      (and (has? player pawns) (or (= player pawn) (guard? pawn))))
                    pawns))))
            (apply concat)))))))

(defn move [state from pawn]
  (if (= -1 from)
    (embark state)
    (continue state from pawn)))

(defn rand-move [state]
  (let [choices (vec (moves state))
        pick    (rand-int (count choices))]
  (if (over? state)
    state
    (apply move state (nth choices pick)))))

(defn play [state choose times] ; make n choices progressing the game state forward
  (reduce choose state (range times)))

(defn sample [choose times & players] ; run a sample game using some move choice algorithm
  (-> {:players (vec players)}
    start
    (play choose times)))

(defn valid? [state]
  (contains? state :players))

(defonce game
  (reagent/atom init :validator valid?))

(defn render-players []
  (apply vector :div
    (map (partial vector :div) (get @game :players))))

(defn src [n]
  (cond
    (pos? n) "images/potion.svg"
    (neg? n) "images/poison.svg"
    (zero? n) "images/tome.svg"))

(defn card [n]
  [:img {:src (src n)}])

(defn render-trail []
  (apply vector :div.trail
    [:img {:src "images/start.svg"}]
    (concat
      (map (partial vector card) (get @game :trail))
      [[:img {:src "images/stop.svg"}]])))

  (reagent/render [render-trail]
    (js/document.getElementById "game"))