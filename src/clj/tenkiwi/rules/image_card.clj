(ns tenkiwi.rules.image-card
  )

(def STATE-KEY :-image-card)

(defn initial-state [starting-state
                     {:keys [images image-key]
                      :as   options}]
  (let [order        (into [] (rest images))
        active-image (first images)
        order-state  {:image-deck   order
                      :image-key    image-key
                      :active-image active-image}]
    (merge
     starting-state
     {STATE-KEY order-state})))


(defn ->active-image [game]
  (get-in game [STATE-KEY :active-image]))

(defn previous-image! [{:as                                         game
                        {:keys [image-deck active-image image-key]} STATE-KEY}]
  (let [new-image      (last image-deck)
        new-image-deck (into [active-image] (pop image-deck))]
      (assoc game
             STATE-KEY {:image-deck   new-image-deck
                        :image-key    image-key
                        :active-image new-image})))

(defn next-image! [{:as                                         game
                    {:keys [image-deck active-image image-key]} STATE-KEY}]
  (let [new-image      (first image-deck)
        new-image-deck (conj (into [] (rest image-deck)) active-image)]
    (assoc game
           STATE-KEY {:image-deck   new-image-deck
                      :image-key    image-key
                      :active-image new-image})))

(defn render-display [state]
  (let [image-key    (get-in state [STATE-KEY :image-key])
        active-image (get-in state [STATE-KEY :active-image])]
    (assoc-in state [:display image-key] active-image)))
