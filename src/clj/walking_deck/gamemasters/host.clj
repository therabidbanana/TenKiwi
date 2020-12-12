(ns walking-deck.gamemasters.host
  "The host is in charge of moving users back and forth to rooms"
  #_(:require [com.stuartsierra.component :as component]))

(def home-room :home)
(defn home-room? [room] (= home-room (or room home-room)))

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

(defn boot-player!
  [{:as system :keys [register]} uid]
  (let [world           (:world register)
        player-location (get-player-location world uid)]
    (send-player-home world uid)
    (let [room (get-room world player-location)]
      (->player system uid [:user/booted!])
      (->room system player-location [:room/user-left! room]))))

#_(defmethod -event-msg-handler :room/join-room!
  [{:keys [gamemaster]} {:as ev-msg :keys [event uid send-fn]}]
  (let [[_ {:keys [user-name room-code]}] event

        rooms        (:rooms gamemaster)
        current-room (@rooms room-code)
        user {:id        uid
              :user-name user-name}]
    (if (= uid :taoensso.sente/nil-uid)
      (println "Warning - unassigned user id! Ignoring join :room/join-room!")
      (do (if current-room
            (swap! rooms update-in [room-code :players] conj user)
            (swap! rooms assoc-in [room-code] {:room-code room-code :players [user]}))
          (swap! (:players gamemaster) assoc uid room-code)))
    (send-fn uid [:user/room-joined! (-> rooms deref (get room-code))])
    (doseq [player (-> rooms deref (get-in [room-code :players]))]
      (send-fn (:id player) [:room/user-joined! (-> rooms deref (get room-code))]))))

#_(defmethod -event-msg-handler :room/boot-player!
  [{:keys [gamemaster]} {:as ev-msg :keys [event uid send-fn]}]
  (let [[_ booted] event

        _ (println booted)
        rooms        (:rooms gamemaster)
        players      (:players gamemaster)
        current-room (@players booted)
        room-code    current-room]
    (if (= uid :taoensso.sente/nil-uid)
      (println "Warning - unassigned user id! Ignoring join :room/join-room!")
      (do (if current-room
            (swap! rooms update-in [room-code :players] (partial remove #(= (:id %) booted))))
          (swap! (:players gamemaster) assoc booted :home)))
    (send-fn booted [:user/booted!])
    (doseq [player (-> rooms deref (get-in [room-code :players]))]
      (send-fn (:id player) [:room/user-left! (-> rooms deref (get room-code))]))))
