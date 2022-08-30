(ns tenkiwi.rules.game-stages)

(defn initial-state [starting-state
                     {:keys [initial-stage stages]
                      :as   options}]
  (let [order-state {:stages       stages
                     :active-stage initial-stage}]
    (merge
     starting-state
     {:-game-stages order-state})))

(defn ->stage-title [{{:keys [active-stage stages]}
                      :-game-stages}]
  (get-in stages [active-stage :title]))

(defn ->stage [{{:keys [active-stage]}
                      :-game-stages}]
  active-stage)

(defn render-display [state]
  (let [active-stage (->stage-title state)
        stage         (->stage state)]
    (-> state
        (update :display assoc :stage stage)
        (update :display assoc :stage-title active-stage))))

(defn change-stage! [game stage]
  (let [updated (-> game
                    (assoc-in [:-game-stages :active-stage] stage))]
    updated))
