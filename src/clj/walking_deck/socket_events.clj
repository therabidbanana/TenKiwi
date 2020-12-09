(ns walking-deck.socket-events
  (:require
            [clojure.core.async :as async :refer [<! <!! >! >!! put! chan go go-loop]]
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
  [{:keys [gamemaster]} {:as ev-msg :keys [uid]}]
  (let [player-location (-> gamemaster :players deref (get uid))]
    (println "Player expected at " (or player-location :unknown))
    #_(println "hiya")))

(defmethod -event-msg-handler :chsk/uidport-open
  [{:keys [gamemaster]} {:as ev-msg :keys [event]}]
  (let [[_ uid] event]
    (if (= uid :taoensso.sente/nil-uid)
      (println "Warning - unassigned user id!")
      (swap! (:players gamemaster) assoc uid :home))
    (println "hiya")))

(defmethod -event-msg-handler :room/join-room!
  [{:keys [gamemaster]} {:as ev-msg :keys [event uid]}]
  (let [[_ {:keys [user-name room-code]}] event

        rooms        (:rooms gamemaster)
        current-room (@rooms room-code)]
    (if (= uid :taoensso.sente/nil-uid)
      (println "Warning - unassigned user id!")
      (do (if current-room
            (swap! rooms update-in [room-code :players] conj user-name)
            (swap! rooms assoc-in [room-code] {:room-code room-code :players [user-name]}))
          (swap! (:players gamemaster) assoc uid room-code)))
    (println "Rooms now -> " @rooms)))

(defmethod -event-msg-handler :example/toggle-broadcast
  [{:keys [broadcast-enabled?_]} {:as ev-msg :keys [?reply-fn]}]
  (let [loop-enabled? (swap! broadcast-enabled?_ not)]
    (?reply-fn loop-enabled?)))

;; TODO Add your (defmethod -event-msg-handler <event-id> [ev-msg] <body>)s here...
