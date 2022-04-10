(ns tenkiwi.rules.x-card
  )

(def STATE-KEY :-x-card)

(defn initial-state [starting-state
                     {:keys []
                      :as   options}]
  (let [extra-state  {:x-card-active? false}]
    (merge
     starting-state
     {STATE-KEY extra-state})))

(defn active? [game]
  (get-in game [STATE-KEY :x-card-active?]))

(defn render-display [current-state]
  (let [x-card? (active? current-state)]
    (-> current-state
        (update :display assoc :x-card-active? x-card?))))

(defn activate-x-card! [game]
  (assoc-in game [STATE-KEY :x-card-active?] true))

(defn reset-x-card! [game]
  (assoc-in game [STATE-KEY :x-card-active?] false))
