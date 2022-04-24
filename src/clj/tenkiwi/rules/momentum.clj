(ns tenkiwi.rules.momentum
  (:require

   [tenkiwi.util :as util :refer [inspect]]
   [tenkiwi.rules.character-sheets :as character-sheets]
   )
  )

(def $ :-momemntum)

(defn initial-state [starting-state
                     {:keys [players score-min score-max default]
                      :or   {score-min 0 score-max 12 default 0}
                      :as   options}]
  (let [scores (reduce
                (fn [a b]
                  (assoc a (:id b) default))
                {}
                players)
        extra-state  {:score-min score-min
                      :scores    scores
                      :score-max score-max}]
    (assoc starting-state $ extra-state)))

(defn ->scores [{{:keys [scores]} $
                 :as              game}]
  scores)

(defn current-score [{{:keys [scores]} $
                      :as              game}
                     path]
  (get-in scores path))

(defn upscore! [{{:keys [scores score-max]} $
                  :as              game}
                 path]
  (let [current-score (get-in scores path)
        new-score (min (inc current-score) score-max)]
    (update-in game
               [$ :scores]
               assoc-in path new-score)))

(defn downscore! [{{:keys [scores score-min]} $
                    :as              game}
                   path]
  (let [current-score (get-in scores path)
        new-score (max (dec current-score) score-min)]
    (update-in game
               [$ :scores]
               assoc-in path new-score)))

(defn render-display [state]
  (-> state
      (update :display assoc :player-momentum (->scores state))))

(defn- build-score-row [name id score]
  {:id id
   :label name
   :text  "Spend 1 momentum to assist (+1), Spend 2 to push yourself (+1)"
   :score score
   :actions [{:text " - "
              :action :downscore-player
              :params {:player-id id}}
             {:text " + "
              :action :upscore-player
              :params {:player-id id}}]})

(defn render-scoreboard-display [state]
  (let [names      (character-sheets/->player-names state)
        scores     (->scores state)
        scoreboard (mapv build-score-row
                         (map names (keys scores))
                         (keys scores)
                         (vals scores))]
    (-> state
        (update :display assoc :player-momentumboard scoreboard))))
