(ns potions-n-poisons.ui
  (:require
    [potions-n-poisons.core :as core]
    [reagent.core :as reagent]))

(defonce game
  (reagent/atom core/init :validator core/valid?))

(def pawn-images [
  "images/guard.svg"
  "images/pawn-red.svg"
  "images/pawn-blue.svg"
  "images/pawn-yellow.svg"
  "images/pawn-green.svg"
  "images/pawn-brown.svg"
  "images/pawn-black.svg"])

(defn render-pawn [n]
  [:img.pawn {:src (get pawn-images (inc n))}])

(defn render-pawns [pawns up from]
  (let [move-guards (get (set pawns) up)]
    [:ol (map
      (fn [n]
        (let [mobile (or (= n up) (and move-guards (core/guard? n)))]
          [(symbol (str "li" (when mobile ".mobile")))
          (when mobile
            {:on-click
              (fn [e]
                (swap! game #(core/move % from n)))})
          [render-pawn n]]))
      pawns)]))

(defn render-card [what worth cured]
  (let [classes (filter some? [what (core/card-kind worth) (when cured "cured")])]
    [(symbol (str "div.card." (clojure.string/join "." classes)))
      [:img.kind {:src (str "images/" (clojure.string.join "-" classes) ".svg")}]
      [:div.worth (or (when worth (if cured (* -1 worth) worth)) "-")]]))

(defn render-space [what idx key worth pawns up]
  [:div.space ^{:key key}
    {:data-key key :data-worth worth}
    [:div.pawns [render-pawns pawns up idx]]
    [render-card what worth]])

(defn render-die [pips]
  (when pips [:img.roll {:src (str "images/die" pips ".svg")}]))

(defn render-players [players up die collected]
  [:table.players>tbody
    (map-indexed
      (fn [idx name]
        (let [coll  (nth collected idx)
              cured (core/cure-poison coll)]
        [:tr
          [:td.name name ]
          [:td.pawn (render-pawn idx)]
          [:td.die (when (= idx up) (render-die die)) ]
          [:td.score (core/score (get collected idx))]
          [:td.collected
            (map-indexed
              (fn [idx worth]
                (render-card nil worth (not= (nth cured idx) worth)))
              coll) ]
          ]))
      players)])

(defn render-player-entry [player-count]
  [:div.entry
    (when (< player-count 6)
      [:div
        [:input {:type "text" :placeholder "Enter player name"}]
        [:button
          {:on-click
            (fn [e]
              (let [parent (.-target.parentNode e)
                    input  (.querySelector parent "input")
                    name   (.-value input)]
                (do
                  (aset input "value" "")
                  (swap! game #(core/join % name))
                  (.focus input))))}
            "Join"]])
    [:button
      {:style {:visibility (if (> player-count 1) "visible" "hidden")}
        :on-click
        (fn [e]
          (swap! game core/start))}
        "Start"]])

(defn render-player-pawn [n players]
  (let [name (nth players n)]
    [:div.player
      (render-pawn n)
      [:div.name name]]))

(defn render-game []
  (let [state @game
        {:keys [players start up die trail collected]} state]
    [:div
      [:h1 "Potions n' Poisons"]
      (when (not up) (render-player-entry (count players)))
      (when (core/over? state)
        [:div.winners
          [:h2 (if (= (count (core/leaders state)) 1) "Winner" "Winners")]
          [:ul
            (map
              #(vector :li.player
                (render-pawn %)
                (vector :div.name (nth players %)))
              (core/leaders state))]])
      (apply vector :div.trail
        [render-space "start" -1 -1 nil start up]
        (concat
          (map-indexed
            (fn [idx]
              (vector render-space nil
                idx
                (get-in state [:ids idx])
                (get-in state [:trail idx])
                (get-in state [:pawns idx])
                up))
            trail)
          [[render-space "stop" 99 "stop"]]))
      [:div.summary
        [render-players players up die collected]]
      ]))

;warn against leaving a game in progress
(aset js/window "onbeforeunload" (constantly "Had too much potion to drink?"))

(reagent/render [render-game]
  (js/document.getElementById "game"))