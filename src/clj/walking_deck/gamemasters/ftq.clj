(ns walking-deck.gamemasters.ftq
  "The host is in charge of moving users back and forth to rooms"
  #_(:require [com.stuartsierra.component :as component]))

;; TODO: Extract these - duplicated in host?
(defn ->players [{:keys [chsk-send!]} uids message]
  (doseq [uid uids]
    (chsk-send! uid message)))

(defn ->player [system uid message]
  (->players system [uid] message))

(defn ->room
  [{:keys [register] :as system} room message]
  (if-let [players (-> register :world deref
                       (get-in [:rooms room :players]))]
    (->players system (map :id players) message)))


#_(defn set-player-room
  ([world-atom uid room-id]
   (let [user-info (get-in @world-atom [:player-info uid])]
     (set-player-room world-atom uid room-id user-info)))
  ([world-atom uid room-id user-info]
   (let [room (or (get-room world-atom room-id)
                  {:id room-id
                   :players []})
         _ (println room)
         new-room (update-in room [:players] conj user-info)]
     (doto world-atom
       (swap! update-in [:players] assoc uid room-id)
       (swap! update-in [:player-info] assoc uid user-info)
       (swap! update-in [:rooms] assoc room-id new-room)))))


(def intro [
            "To play, each player in turn will receive a card to read aloud"
            ])

;; TODO: XSS danger?
(defn waiting-for
  [{:keys [user-name]}]
  (str "Waiting for " user-name "..."))

(defn start-game [world-atom room-id]
  (let [players      (get-in @world-atom [:rooms room-id :players])
        player-order (mapv :id players)
        new-game     {:player-order     player-order
                      :game             :ftq
                      :state            :intro
                      :discard          []
                      :active-player    (first player-order)
                      :active-display   {:card (first intro)
                                         :actions [:done :pass]}
                      :inactive-display {:card (waiting-for (first players))}}]
    (doto world-atom
      (swap! update-in [:rooms room-id] assoc :game new-game))))


(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
