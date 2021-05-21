(ns tenkiwi.gamemasters.oracle
  "Oracle is a gamemaster supporting generic story telling"
  (:require [tenkiwi.util :as util :refer [inspect]]))

(def done-action
  {:action :done
   :text   "Finish Turn"})

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

(defn- find-next-player [{:keys [players-by-id]
                          :as   game-state}]
  (let [all-players (next-player-order game-state)
        next-player (get players-by-id (first all-players))]
    next-player))

(defn build-active-display [{:keys [players
                                    generators]
                             :as   game-state}]
  (let [next-player (find-next-player game-state)]
    {:card          {:id    "waiting"
                     :type :inactive
                     :text  (str "It is " (:user-name next-player) "'s turn...")}
     :extra-details (map #(hash-map :title %
                                    :name %
                                    :items (take 3 (shuffle (mapv :text (get generators % [])))))
                         (keys generators))
     :extra-actions [leave-game-action]
     :actions       [done-action (pass-action next-player)]}))

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
  (let [{:keys [generator]
         :as   decks} (inspect (util/gather-decks (inspect game-url)))

        _ (inspect decks)
        player-ids    (map :id players)
        initial-state (inspect {:player-order-ids (into [] (rest player-ids))
                        :players-by-id    (util/index-by :id players)
                        :active-player-id (first player-ids)
                        :game-type        :oracle
                        :state            :intro
                        :available-cards  decks
                        :generators       (group-by :group generator)})]
    (println initial-state)
    (merge initial-state
           {:active-display   (build-active-display initial-state)
            :inactive-display (build-inactive-display initial-state)})))

(defn leave-game [game-state]
  nil)

(defmulti do-action (fn [{:keys [uid room-id action params]} game-state]
                      action))

(defmethod do-action :leave-game
  [{:keys [uid room-id action params]} game-state]
  nil)

(defmethod do-action :tick-clock
  [{:keys [uid room-id action params]} game-state]
  :no-op)

(defmethod do-action :default
  [{:keys [uid room-id action params]} game-state]
  (println (str "Unknown action " action))
  game-state)

(defn take-action [action room]
  (let [next-state (do-action action (:game room))]
    (cond
      (= :no-op next-state) (:game room)
      (nil? next-state) nil
      next-state
      (merge next-state
            {:active-display   (build-active-display next-state)
             :inactive-display (build-inactive-display next-state)}))))
