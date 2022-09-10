(ns tenkiwi.rules.game-stages)

(def $ :-game-stages)

(defn initial-state [starting-state
                     {:keys [initial-stage stages phases]
                      :as   options}]
  (let [order-state {:stages       stages
                     :phases       phases
                     :active-phase (first (get stages [initial-stage :phases]))
                     :active-stage initial-stage}]
    (merge
     starting-state
     {$ order-state})))

(defn ->stage-title [{{:keys [active-stage stages]} $}]
  (get-in stages [active-stage :title]))

(defn ->stage [{{:keys [active-stage]} $}]
  active-stage)

(defn ->phase [{{:keys [active-phase]} $}]
  active-phase)

(defn render-display [state]
  (let [active-stage (->stage-title state)
        stage         (->stage state)
        phase         (->phase state)
        ]
    (-> state
        (update :display assoc :stage stage)
        (update :display assoc :phase phase)
        (update :display assoc :stage-title active-stage))))

(defn change-stage!
  ([game stage]
   (change-stage! game stage identity))
  ([game stage turn-start]
   (let [new-stage (get-in game [$ :stages stage])
         new-phase (first (get-in new-stage [:phases] []))
         updated   (-> game
                       (assoc-in [$ :active-phase] new-phase)
                       (assoc-in [$ :active-stage] stage))]
     (if (get-in game [$ :stages stage])
       (turn-start updated)
       game))))

(defn next-phase!
  ([game]
   (next-phase! game identity))
  ([game turn-start]
   (let [phase-list (get-in game [$ :stages (->stage game) :phases] [])
         next-phase (->> phase-list
                         (drop-while #(not= (->phase game) %))
                         rest
                         first)]
     (cond-> game
       (nil? next-phase)
       (turn-start)
       :always
       (assoc-in [$ :active-phase] (or next-phase (first phase-list)))))))
