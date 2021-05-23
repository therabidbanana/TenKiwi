(ns tenkiwi.gamemasters.oracle
  "Oracle is a gamemaster supporting generic story telling"
  (:require [tenkiwi.util :as util :refer [inspect]]))

(def done-action
  {:action :done
   :text   "Finish Turn"})

(def reclaim-cards-action
  {:action :reclaim-cards
   :text   "Reclaim Cards"})

(def leave-game-action
  {:action  :leave-game
   :confirm true
   :text    "End Game Now"})

(defn pass-action [next-player]
  {:action :pass
   :text   (str "Pass card to " (:user-name next-player))})

(defn- next-player-order [{:keys [player-order-ids
                                  active-player-id]}]
  (conj player-order-ids active-player-id))

(defn- next-player-state [{:keys [players-by-id] :as game-state}]
  (let [next-order (next-player-order game-state)]
    {:player-order-ids (into [] (rest next-order))
     :active-player-id (first next-order)
     :active-player    (get players-by-id (first next-order))}))

(defn- find-next-player [{:as   game-state}]
  (:active-player (next-player-state game-state)))

(defn draw-from-deck-action [[deck-name cards]]
  (if (> (count cards) 0)
    {:text   (str "Draw from " deck-name)
     :action :draw-card
     :params {:deck deck-name}}
    {:text     (str "Draw from " deck-name " [empty]")
     :disabled true
     :action   :draw-card
     :params   {:deck deck-name}}))

(defn generate-something-action [[deck-name cards]]
  {:text   (str "Generate " deck-name)
   :action :generate-something
   :params {:builder deck-name}})

(defn build-active-display [{:keys [available-cards
                                    active-player
                                    generators
                                    builders
                                    discard]
                             :as   game-state}]
  (let [next-player (find-next-player game-state)
        active-card (or (first discard)
                        {:id    "waiting"
                         :type :inactive
                         :text  (str "It is " (:user-name active-player) "'s turn...")})
        ]
    {:card          active-card
     :extra-details (map #(hash-map :title %
                                    :name %
                                    :items (take 3 (shuffle (mapv :text (get generators % [])))))
                         (keys generators))
     :extra-actions (concat
                     [reclaim-cards-action]
                     (mapv draw-from-deck-action available-cards)
                     (mapv generate-something-action builders)
                     [leave-game-action])
     :actions       [done-action #_(pass-action next-player)]}))

(defn build-inactive-display
  ([game-state active-version]
   (merge active-version
          {:actions []}))
  ([game-state] (build-inactive-display game-state (build-active-display game-state))))

(defn start-game [room-id
                  {:keys [game-url]
                   :or   {game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=1204467298&single=true&output=tsv"}}
                  {:keys [players]
                   :as   game}]
  (let [{:keys [generator
                builder]
         :as   decks} (util/gather-decks game-url)

        player-ids    (map :id players)
        initial-state {:player-order-ids (into [] (rest player-ids))
                       :players-by-id    (util/index-by :id players)
                       :active-player-id (first player-ids)
                       :active-player    (first players)
                       :discard          []
                       :game-type        :oracle
                       :state            :intro
                       :available-cards  (dissoc decks
                                                 :generator
                                                 :builder)
                       :generators       (group-by :group generator)
                       :builders         (group-by :group builder)}]
    (println initial-state)
    (merge initial-state
           {:active-display   (build-active-display initial-state)
            :inactive-display (build-inactive-display initial-state)})))

(defn leave-game [game-state]
  nil)

(defmulti do-action (fn [{:keys [uid room-id action params]}
                         {:keys [active-player-id] :as game-state}]
                      [(if (= uid active-player-id)
                         :active-> :inactive->)
                       action]))

(defmethod do-action [:active-> :leave-game]
  [{:keys [uid room-id action params]} game-state]
  nil)

(defmethod do-action [:inactive-> :leave-game]
  [{:keys [uid room-id action params]} game-state]
  nil)

(defmethod do-action [:active-> :done]
  [{:keys [uid room-id action params]}
   {:keys [player-order active-player-id]
    :as   game-state}
   ]
  (let []
    (merge game-state (next-player-state game-state))))

(defmethod do-action [:active-> :reclaim-cards]
  [{:keys [uid room-id action params]}
   {:keys [available-cards discard active-player-id]
    :as   game-state}]
  (let [deck-name       (:deck params)
        card-filter     (if deck-name
                          #(= deck-name (:type %))
                          #((set (keys available-cards)) (:type %)))
        all-cards       (concat (mapcat second available-cards)
                                (filterv card-filter discard))
        new-discard     (remove card-filter discard)]
    (assoc game-state
           :available-cards (group-by :type all-cards)
           :discard new-discard)))

(defmethod do-action [:active-> :draw-card]
  [{:keys [uid room-id action params]}
   {:keys [available-cards discard]
    :as   game-state}]
  (let [deck-name       (:deck params)
        working         (-> (get available-cards deck-name [])
                            shuffle)
        next-card       (first working)
        available-cards (assoc available-cards
                               deck-name (rest working))]
    (if deck-name
      (assoc game-state
            :available-cards available-cards
            :discard (cons next-card discard))
      :no-op)))

(defmethod do-action [:active-> :generate-something]
  [{:keys [uid room-id action params]}
   {:keys [builders generators discard]
    :as   game-state}]
  (let [deck-name      (:builder params)
        {:keys [text extras]
         :or   {extras ""}
         :as   card}   (-> (get builders deck-name [])
                           shuffle
                           first)
        generator-list (->> (clojure.string/split extras #"\s\s+")
                            (map #(clojure.string/split % #":"))
                            (into {}))

        next-card (merge card
                         {:id     :builder
                          :inputs (mapv #(hash-map :name (first %)
                                                   :label (last %)
                                                   :value (util/pluck-text generators (first %)))
                                        generator-list)})]
    (if deck-name
      (assoc game-state
             :discard (cons next-card discard))
      :no-op)))

(defmethod do-action [:inactive-> :done]
  [_ _] :no-op)

(defmethod do-action [:inactive-> :reclaim-cards]
  [_ _] :no-op)

(defmethod do-action [:inactive-> :draw-card]
  [_ _] :no-op)

(defmethod do-action [:inactive-> :generate-something]
  [_ _] :no-op)

(defmethod do-action [:inactive-> :tick-clock]
  [_ _] :no-op)

(defmethod do-action :default
  [{:keys [uid room-id action params]} game-state]
  (println (str "Unknown action " action))
  game-state)

(defn take-action [action {game-state :game}]
  (let [next-state (do-action action game-state)]
    (cond
      (= :no-op next-state) game-state
      (nil? next-state) nil
      next-state
      (merge next-state
            {:active-display   (build-active-display next-state)
             :inactive-display (build-inactive-display next-state)}))))
