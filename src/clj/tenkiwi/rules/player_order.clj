(ns tenkiwi.rules.player-order)

(defn- previous-player-by-order [player-order current-player]
  (let [curr-id    (:id current-player)
        curr-index (.indexOf (mapv :id player-order) curr-id)
        prev-index (dec curr-index)
        prev-index (if (> 0 prev-index)
                     (dec (count player-order))
                     prev-index)]
    (nth player-order prev-index)))

(defn- next-player-by-order [player-order current-player]
  (let [curr-id    (:id current-player)
        curr-index (.indexOf (mapv :id player-order) curr-id)
        next-index (inc curr-index)
        next-index (if (>= next-index (count player-order))
                     0
                     next-index)]
    (nth player-order next-index)))

(defn initial-state [starting-state
                     {:keys [players]
                      :as   options}]
  (let [order (into [] players)
        active-player (first players)
        next-player (next-player-by-order order active-player)
        order-state {:order         order
                     ;; For turns with multiple phases, first player changes per
                     ;; turn
                     :first-player  active-player
                     :active-player active-player}]
    (merge
     starting-state
     {:-player-order order-state})))

(defn everyone-done? [{{:keys [active-player
                               first-player
                               order]}
                       :-player-order}]
  (= (:id (next-player-by-order order active-player))
     (:id first-player)))

(defn next-player [{{:keys [active-player
                            order]}
                    :-player-order}]
  (next-player-by-order order active-player))

(defn previous-player [{{:keys [active-player
                                order]}
                        :-player-order}]
  (previous-player-by-order order active-player))

(defn active-player [{{:keys [active-player]}
                      :-player-order}]
  active-player)

(defn player-order [{{:keys [order]}
                     :-player-order}]
  order)

(defn player-by-id [{{:keys [order]}
                     :-player-order}
                    id]
  (first (filter #(#{id} (:id %)) order)))

(defn render-display [state]
  (let [active-player (active-player state)
        next-player   (next-player state)]
    (-> state
        (update :display assoc :active-player active-player)
        (update :display assoc :next-player next-player))))

(defn is-active? [{{:keys [active-player]}
                   :-player-order}
                  {:keys [id]}]
  (= id (:id active-player)))

(defn activate-next-player! [{:keys [-player-order]
                              :as   game}]
  (let [next-up (next-player game)
        updated (-> game
                    (assoc-in [:-player-order :active-player] next-up))]
    updated))

;; Phased turns restart each phase with the first player, then advance
;; first-player when there is a a new turn.
(defn next-first-player [{{:keys [first-player
                                  order]}
                          :-player-order}]
  (next-player-by-order order first-player))

(defn first-player [{{:keys [first-player]}
                     :-player-order}]
  first-player)

(defn activate-next-phase! [{:keys [-player-order]
                              :as   game}]
  (let [next-up (first-player game)
        updated (-> game
                    (assoc-in [:-player-order :active-player] next-up))]
    updated))

(defn activate-next-turn! [{:keys [-player-order]
                             :as   game}]
  (let [next-up (next-first-player game)
        updated (-> game
                    (assoc-in [:-player-order :first-player] next-up)
                    (assoc-in [:-player-order :active-player] next-up))]
    updated))
