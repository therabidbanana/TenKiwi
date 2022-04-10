(ns tenkiwi.rules.voteboard
  (:require

   [tenkiwi.util :as util :refer [inspect]]
   )
  )

(def $ :-voteboard)

(defn- build-starting-scores [{:keys [npc? id]} players default]
  (let [ids (remove #(= id %) (map :id players))]
    (zipmap ids (cycle [default]))))

;; (build-starting-scores {:id 'a}
;;                        [{:id 'a} {:id 'b} {:id 'c :npc? true}]
;;                        5)

(defn initial-state [starting-state
                     {:keys [players score-min score-max default]
                      :or   {score-min 1 score-max 10 default 5}
                      :as   options}]
  (let [scores (reduce
                (fn [a b]
                  (assoc a
                         (:id b)
                         (build-starting-scores b (remove :npc? players) default)))
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

(defn upvote! [{{:keys [scores score-max]} $
                :as              game}
               path]
  (let [current-score (get-in scores path)
        new-score (min (inc current-score) score-max)]
    (update-in game
               [$ :scores]
               assoc-in path new-score)))

(defn downvote! [{{:keys [scores score-min]} $
                  :as              game}
                 path]
  (let [current-score (get-in scores path)
        new-score (max (dec current-score) score-min)]
    (update-in game
               [$ :scores]
               assoc-in path new-score)))

(defn ->total-scores [{{:keys [scores]} $
                       :as                     game}]
  (let [sum-scores  #(apply + (vals %))
        base-scores (zipmap (keys scores)
                            (map sum-scores (vals scores)))
        ;; final-scores (reduce #(update %1 %2 + (score-ranks %2 player-ranks))
        ;;                      base-scores
        ;;                      (keys base-scores))
        ]
    base-scores))

(defn render-display [state]
  (-> state
      (update :display assoc :player-scores (->scores state))
      (update :display assoc :total-scores (->total-scores state))))

;; (initial-state {} {:players [{:id 'a} {:id 'b} {:id 'c :npc? true}]
;;                    })
