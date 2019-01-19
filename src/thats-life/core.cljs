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

(defn over? [state]
  (empty? (in-play state)))

(defn up [state]
  (get state :up))

(defn remove-once [pred xs]
  (let [[ys zs] (split-with (complement pred) xs)]
    (vec (concat ys (rest zs)))))

(defn unconj [xs x]
  (remove-once (partial = x) xs))

(defn start [state]
  (let [player-count (count (get state :players))]
    (-> state
      (assoc :start (vec (mapcat #(repeat (starting-pawns player-count) %) (range player-count))))
      (assoc :trail init-trail)
      (assoc :pawns (spots init-trail))
      (assoc :collected (vec (repeat player-count [])))
      (assoc :up (rand-int player-count))
      (assoc :die (roll)))))

(defn next-up [state]
  (let [player  (get state :up)
        players (playing state)]
    (-> state
      (assoc :up
        (when (not (over? state))
          (second (drop-while #(not= player %) (cycle players)))))
      (assoc :die (roll)))))

(defn has? [x xs]
  (some #{x} xs))

(defn collect [state from]
  (let [player (up state)]
    (if (empty? (get-in state [:pawns from]))
      (-> state
        (update-in [:collected player] #(conj % (nth (get state :trail) from)))
        (update :trail
          (fn [trail]
            (let [[xs ys] (split-at from trail)]
              (vec (concat xs (rest ys)))))))
      state)))

(defn embark [state]
  (let [player (up state)]
    (if ((set (unmoved state)) player)
      (-> state
        (update :start #(unconj (fn [x] (= player x)) %))
        (update-in [:pawns (- (get state :die) 1)] #(conj % player))
        next-up)
      state)))

(defn continue [state from pawn]
  (let [player (up state)
        pawns  (get-in state [:pawns from])
        to     (+ from (get state :die))
        exited (> to (- (count (get state :trail)) 1))
        take   #(unconj % pawn)
        put    (if exited identity #(conj % pawn))]
    (if (and (has? player pawns) (or (= player pawn) (guard? pawn)))
      (-> state
        (update-in [:pawns from] take)
        (update-in [:pawns to  ] put)
        (collect from)
        next-up)
      state)))

(defn moves
  ([state pawn]
    (filter #(= pawn (second %)) (moves state)))
  ([state]
    (let [player (up state)]
      (distinct
        (apply concat
          (map-indexed
            (fn [idx pawns]
              (map
                #(vector idx %)
                (filter
                  (fn [pawn]
                    (and (has? player pawns) (or (= player pawn) (guard? pawn))))
                  pawns)))
            (get state :pawns)))))))

(defn move [state from pawn]
  (if (= -1 from)
    (embark state)
    (continue state from pawn)))

(defonce game
  (atom init))