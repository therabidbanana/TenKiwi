(ns walking-deck.components.sente
  (:require [com.stuartsierra.component :as component]
            [taoensso.sente :as sente]))

(defrecord ChannelSocketClient [path csrf-token options]
  component/Lifecycle
  (start [component]
    (let [handler (get-in component [:sente-handler :handler])
          {:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket-client! path csrf-token options)
          component (assoc component
                           :chsk chsk
                           :ch-chsk ch-recv ; ChannelSocket's receive channel
                           :chsk-send! send-fn ; ChannelSocket's send API fn
                           :chsk-state state)]
      (if handler
        (assoc component :router (sente/start-chsk-router! ch-recv handler))
        component)))
  (stop [component]
    ;; Bugfix on this line - submit upstream?
    (when-let [chsk (:chsk component)]
      (sente/chsk-disconnect! chsk))
    (when-let [stop-f (:router component)]
      (stop-f))
    (assoc component
           :router nil
           :chsk nil
           :ch-chsk nil
           :chsk-send! nil
           :chsk-state nil)))

(defn new-channel-socket-client
  ([csrf-token]
   (new-channel-socket-client "/chsk" csrf-token))
  ([path csrf-token]
   (new-channel-socket-client path csrf-token {}))
  ([path csrf-token options]
   (map->ChannelSocketClient {:path path
                              :csrf-token csrf-token
                              :options options})))
