(ns thats-life.core)

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

(defn spots [trail]
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

(defn join [name state]
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

(defn over? [state]
  (empty? (in-play state)))

(defn remove-once [pred xs]
  (let [[ys zs] (split-with (complement pred) xs)]
    (vec (concat ys (rest zs)))))

(defn start [state]
  (let [player-count (count (get state :players))]
    (-> state
      (assoc :start (vec (mapcat #(repeat (starting-pawns player-count) %) (range player-count))))
      (assoc :trail init-trail)
      (assoc :pawns (spots init-trail))
      (assoc :collected (vec (repeat player-count [])))
      (assoc :up (rand-int player-count))
      (assoc :die (roll)))))

(defn end-turn [state]
  (let [player  (get state :up)
        players (playing state)]
    (assoc state :up
      (when (not (over? state))
        (second (drop-while #(not= player %) (cycle players)))))))

(defn embark [state]
  (let [player (get state :up)]
    (if ((set (unmoved state)) player)
      (-> state
        (update :start #(remove-once (fn [x] (= player x)) %))
        (update-in [:pawns (- (get state :die) 1)] #(conj % player))
        end-turn)
      state)))

(defonce game
  (atom init))