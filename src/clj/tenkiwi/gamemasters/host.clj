(ns tenkiwi.gamemasters.host
  "The host is in charge of moving users back and forth to rooms"
  (:require [tenkiwi.gamemasters.ftq :as ftq]
            [tenkiwi.gamemasters.debrief :as debrief]
))

(def home-room :home)
(defn home-room? [room] (= home-room (or room home-room)))
(defn valid-game? [type] (#{:ftq :debrief} type))

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

(defn get-player-location [world-atom uid]
  (get-in @world-atom [:players uid]))

(defn get-room [world-atom room-id]
  (get-in @world-atom [:rooms room-id]))

(defn send-player-home [world-atom uid]
  (let [player-location (get-player-location world-atom uid)
        filter-user (fn [list] (remove #(= uid (:id %)) list))]
    (if-not (home-room? player-location)
      (doto world-atom
        (swap! update-in [:players] assoc uid home-room)
        (swap! update-in [:rooms player-location :players] filter-user)))
    world-atom))

(defn set-player-room
  ([world-atom uid room-id]
   (let [user-info (get-in @world-atom [:player-info uid])]
     (set-player-room world-atom uid room-id user-info)))
  ([world-atom uid room-id user-info]
   (let [room (or (get-room world-atom room-id)
                  {:id room-id
                   :room-code room-id
                   :players []})
         _ (println room)
         new-room (update-in room [:players] conj user-info)]
     (doto world-atom
      (swap! update-in [:players] assoc uid room-id)
      (swap! update-in [:player-info] assoc uid user-info)
      (swap! update-in [:rooms] assoc room-id new-room)))))

#_(send-player-home (atom {:players {1 1} :rooms {1 {:players [{:id 1}]}}}) 1)

#_(set-player-room (atom {}) 1 "a" {:id 1 :user-name "foo"})

(defn new-arrival!
  "Called whenever a new uid arrives"
  [{:as system :keys [register chsk-send!]} uid]
  (let [player-location   (get-player-location (:world register) uid)
        invalid-redirect? (or (= uid :taoensso.sente/nil-uid) (home-room? player-location))]
    (println chsk-send! uid player-location)
    (if-not invalid-redirect?
      (if-let [room (get-room (:world register) player-location)]
        (->player system uid [:user/room-joined! room])))))

(defn leave-room!
  [{:as system :keys [register]} uid]
  (send-player-home (:world register) uid))

(defn join-room!
  "Called when a player tries to join an existing room"
  [{:as system :keys [register]} uid room-code {:as join-info :keys [user-name]}]
  (let [world           (:world register)
        player-location (get-player-location world uid)]
    (if-not (home-room? player-location)
      (leave-room! system uid))
    (set-player-room world uid room-code join-info)
    (let [player-location (get-player-location world uid)
          room        (get-room world player-location)]
      (->player system uid [:user/room-joined! room])
      (println "send to " player-location)
      (->room system player-location [:room/user-joined! room]))))

(defn start-game!
  "Called to trigger a game start by host"
  [{:as system :keys [register]} uid {:keys [game-type
                                             params]}]
  (let [world           (:world register)
        player-location (get-player-location world uid)
        room            (get-room world player-location)]
    (cond
      (home-room? player-location)  nil
      (not (valid-game? game-type)) nil
      :else
      (do
        (try
          (case game-type
            :ftq (ftq/start-game world player-location params)
            :debrief (debrief/start-game world player-location)
            ;; call game setup
            )
          (catch Exception e (println e)))
        (->room system player-location [:game/started! (get-room world player-location)])))))

(defn tick-clock!
  "Called by the system to tick all game clocks"
  [{:as system :keys [register]}]
  (doseq [room (-> register :world deref :rooms keys)]
    (let [world        (:world register)
          current-game (get-in (get-room world room)
                               [:game :game-type])
          action       {:room-id room
                        :action  :tick-clock
                        :uid     :timekeeper}]
      (cond
        (not (valid-game? current-game)) nil
        :else
        true ;; Nothing ticks right now
        #_(do
          (case current-game
            ;; :ftq          (ftq/take-action world action)
            :debrief (debrief/take-action world action)
            ;; call game setup
            )
          (->room system room [:game/changed! (get-room world room)]))))))

(defn take-action!
  "Called to trigger a game start by host"
  [{:as system :keys [register]} uid action]
  (let [world           (:world register)
        player-location (get-player-location world uid)
        room            (get-room world player-location)
        current-game    (get-in room [:game :game-type])
        action          (assoc action :room-id player-location :uid uid)]
    (println action)
    (cond
      (home-room? player-location)  nil
      (not (valid-game? current-game)) nil
      :else
      (do
        (case current-game
          :ftq (ftq/take-action world action)
          :debrief (debrief/take-action world action)
          ;; call game setup
          )
        (->room system player-location [:game/changed! (get-room world player-location)])))))

(defn boot-player!
  [{:as system :keys [register]} uid]
  (let [world           (:world register)
        player-location (get-player-location world uid)]
    (send-player-home world uid)
    (let [room (get-room world player-location)]
      (->player system uid [:user/booted!])
      (->room system player-location [:room/user-left! room]))))
