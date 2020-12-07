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
  [{:keys []} {:as ev-msg :keys []}]
  (let []
    #_(println "hiya")))

(defmethod -event-msg-handler :example/toggle-broadcast
  [{:keys [broadcast-enabled?_]} {:as ev-msg :keys [?reply-fn]}]
  (let [loop-enabled? (swap! broadcast-enabled?_ not)]
    (?reply-fn loop-enabled?)))

;; TODO Add your (defmethod -event-msg-handler <event-id> [ev-msg] <body>)s here...
