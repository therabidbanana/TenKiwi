(ns walking-deck.socket-events
  (:require
            [clojure.core.async :as async :refer [<! <!! >! >!! put! chan go go-loop]]
            [walking-deck.gamemasters.host :as host]
            [taoensso.sente :as sente]))

;; (reset! sente/debug-mode?_ true)                            ; Uncomment for extra debug info

;;;; Sente event handlers

(defmulti -event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  #(:id %2)                                              ; Dispatch on event-id
  )

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [component]
  (fn [ev-msg]
    (println "Incoming event is : " (:id ev-msg) (:?data ev-msg))
    (-event-msg-handler component ev-msg)                             ; Handle event-msgs on a single thread
    ;; (future (-event-msg-handler component ev-msg)) ; Handle event-msgs on a thread pool
    ))

(defmethod -event-msg-handler
  :default                                                  ; Default/fallback case (no other matching handler)
  [component {:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (let [session (:session ring-req)
        uid (:uid session)]
    (println "Unhandled event: %s" event)
    (when ?reply-fn
      (?reply-fn {:umatched-event-as-echoed-from-from-server event}))))

(defmethod -event-msg-handler :chsk/ws-ping
  [{:keys [register]} {:as ev-msg :keys [uid send-fn]}]
  (let []
    #_(host/player-ping register uid)
    #_(if-let [player-location (-> gamemaster :players deref (get uid))]
      (send-fn uid [:user/room-joined! (-> rooms deref (get player-location))])
     #_(println "Player expected at " (or player-location :unknown))
     #_(println "hiya"))))

(defmethod -event-msg-handler :chsk/uidport-open
  [system {:as ev-msg :keys [uid event send-fn]}]
  (let []
    #_(host/new-arrival! system uid)))

(defmethod -event-msg-handler :user/connected!
  [system {:as ev-msg :keys [uid event send-fn]}]
  (host/new-arrival! system uid))

(defmethod -event-msg-handler :game/start!
  [system {:as ev-msg :keys [uid event send-fn]}]
  (let [[_ game-type] event]
    (host/start-game! system uid game-type)
    (println "ready to start")))

(defmethod -event-msg-handler :game/action!
  [system {:as ev-msg :keys [uid event send-fn]}]
  (let [[_ action] event]
    (host/take-action! system uid action)))

(defmethod -event-msg-handler :room/join-room!
  [system {:as ev-msg :keys [event uid send-fn]}]
  (let [[_ {:keys [user-name room-code]}] event

        user {:id        uid
              :user-name user-name}]
    (if (= uid :taoensso.sente/nil-uid)
      (println "Warning - unassigned user id! Ignoring join :room/join-room!")
      (host/join-room! system uid room-code user))))

(defmethod -event-msg-handler :room/boot-player!
  [system {:as ev-msg :keys [event uid send-fn]}]
  (let [[_ booted] event]
    (if (= uid :taoensso.sente/nil-uid)
      (println "Warning - unassigned user id! Ignoring boot")
      (host/boot-player! system booted))))
