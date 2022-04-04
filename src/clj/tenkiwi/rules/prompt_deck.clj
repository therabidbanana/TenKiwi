(ns tenkiwi.rules.prompt-deck
  )

(def STATE-KEY :-prompt-deck)

(defn initial-state [starting-state
                     {:keys [deck discard]
                      :or   {discard []}
                      :as   options}]
  (let [order        (into [] deck)
        discard      (into [] discard)
        order-state  {:deck        (rest order)
                      :discard     discard
                      :active-card (first order)}]
    (merge
     starting-state
     {STATE-KEY order-state})))

(defn active-card [{:as                                game
                    {:keys [deck discard active-card]} STATE-KEY}]
  (get-in game [STATE-KEY :active-card]))

(defn draw-next-card! [{:as                                game
                        {:keys [deck discard active-card]} STATE-KEY}]
  (let [new-active  (first deck)
        new-discard (cons active-card discard)
        new-deck    (into [] (rest deck))]
    (assoc game
           STATE-KEY {:deck        new-deck
                      :discard     new-discard
                      :active-card new-active})))

#_(defn next-image! [{:as                                         game
                    {:keys [image-deck active-image image-key]} STATE-KEY}]
  (let [new-image      (first image-deck)
        new-image-deck (conj (into [] (rest image-deck)) active-image)]
    (assoc game
           STATE-KEY {:image-deck   new-image-deck
                         :image-key    image-key
                         :active-image new-image}
           image-key new-image)))
