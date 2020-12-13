(ns walking-deck.socket-events
  (:require [com.stuartsierra.component :as component]
            [re-frame.core :as re-frame]
            [goog.string :as gstr]))

(defn ->output! [fmt & args]
  (let [msg (apply gstr/format fmt args)]
    (println msg)))

;;;; Sente event handlers
(defmulti -event-msg-handler
          "Multimethod to handle Sente `event-msg`s"
          :id ; Dispatch on event-id
          )

(defn event-msg-handler
      "Wraps `-event-msg-handler` with logging, error catching, etc."
      [{:as ev-msg :keys [id ?data event]}]
      (-event-msg-handler ev-msg))

(defmethod -event-msg-handler
           :default ; Default/fallback case (no other matching handler)
           [{:as ev-msg :keys [event]}]
           (->output! "Unhandled event: %s" event))

(defmethod -event-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]}]
  (let [[old-state-map new-state-map] ?data]
    (if (:first-open? new-state-map)
      (do
        (re-frame/dispatch [:user/connected!])
        (->output! "Channel socket successfully established!: %s" new-state-map))
      (->output! "Channel socket state change: %s"              new-state-map))))

(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data]}]
  (re-frame/dispatch ?data)
  (->output! "Push event from server: %s" ?data))

(defmethod -event-msg-handler :chsk/handshake
           [{:as ev-msg :keys [?data]}]
           (let [[?uid ?csrf-token ?handshake-data] ?data]
             (re-frame/dispatch [:initialize-system])))

(defmethod -event-msg-handler :chsk/ws-ping
  [{:as ev-msg :keys [?data]}]
  (let [[?uid ?csrf-token ?ping-data] ?data]
    (->output! "ping: %s" ?data)))
