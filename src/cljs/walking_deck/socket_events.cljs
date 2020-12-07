(ns walking-deck.socket-events
  (:require [com.stuartsierra.component :as component]
            [taoensso.sente  :as sente  :refer (cb-success?)]
            [goog.string         :as gstr]
            [walking-deck.components.ui :refer [new-ui-component]]) )

(def output-el (.getElementById js/document "output"))
(defn ->output! [fmt & args]
  (let [msg (apply gstr/format fmt args)]
    (aset output-el "value" (str "• " (.-value output-el) "\n" msg))
    (aset output-el "scrollTop" (.-scrollHeight output-el))))


(->output! "ClojureScript appears to have loaded correctly.")

;;;; Define our Sente channel socket (chsk) client
(def ?csrf-token
  (when-let [el (.getElementById js/document "sente-csrf-token")]
    (.getAttribute el "data-csrf-token")))

(let [;; Serializtion format, must use same val for client + server:
      packer :edn ; Default packer, a good choice in most cases
      ;; (sente-transit/get-transit-packer) ; Needs Transit dep

      {:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket-client!
        "/chsk" ; Must match server Ring routing URL
        ?csrf-token
        {:type   :auto
         :packer packer})]

     (def chsk       chsk)
     (def ch-chsk    ch-recv) ; ChannelSocket's receive channel
     (def chsk-send! send-fn) ; ChannelSocket's send API fn
     (def chsk-state state)   ; Watchable, read-only atom
     )

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
                  (->output! "Channel socket successfully established!: %s" new-state-map)
                  (->output! "Channel socket state change: %s"              new-state-map))))

(defmethod -event-msg-handler :chsk/recv
           [{:as ev-msg :keys [?data]}]
           (->output! "Push event from server: %s" ?data))

(defmethod -event-msg-handler :chsk/handshake
           [{:as ev-msg :keys [?data]}]
           (let [[?uid ?csrf-token ?handshake-data] ?data]
                (->output! "Handshake: %s" ?data)))
