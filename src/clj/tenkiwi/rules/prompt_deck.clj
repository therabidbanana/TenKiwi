(ns tenkiwi.rules.prompt-deck
  (:require [tenkiwi.rules.player-order :as player-order]))

(def STATE-KEY :-prompt-deck)

(defn initial-state [starting-state
                     {:keys [deck discard features]
                      :or   {discard [] features {}}
                      :as   options}]
  (let [order        (into [] deck)
        discard      (into [] discard)
        order-state  {:deck        (rest order)
                      :features    features
                      :discard     discard
                      :active-card (first order)}]
    (merge
     starting-state
     {STATE-KEY order-state})))

(defn active-card [{:as                           game
                    {:keys [flags deck
                            discard active-card]} STATE-KEY}]
  (merge (get-in game [STATE-KEY :active-card])
         flags))

(defn flag-card! [{:as                           game
                   {:keys [flags deck
                           discard active-card]} STATE-KEY}
                  new-flags]
  (update-in game [STATE-KEY :flags] merge new-flags))

(defn card-passed! [game] (flag-card! game {:passed? true}))

(defn build-feature-flags [game flags]
  (cond-> {}
    (:everyone flags)
    (assoc :starting-player (player-order/active-player game))))

(defn ->everyone? [game]
  (let [up-next (player-order/next-player game)
        tags    (get-in game [STATE-KEY :active-card :tags])
        starter (get-in game [STATE-KEY :flags :starting-player])
        passed? (get-in game [STATE-KEY :flags :passed?])]
    (or (not (:everyone tags))
        (and passed? (= starter up-next)))))

(defn draw-next-card! [{:as                                game
                        {:keys [features deck
                                discard active-card]} STATE-KEY}]
  (let [new-flags   (build-feature-flags game features)
        new-active  (first deck)
        new-discard (cons active-card discard)
        new-deck    (into [] (rest deck))]
    (assoc game
           STATE-KEY {:deck        new-deck
                      :flags       new-flags
                      :features    features
                      :discard     new-discard
                      :active-card new-active})))
