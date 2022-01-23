(ns tenkiwi.components.register
  "Used for keeping track of gamestate across all players. The register is
  updated by gamemasters, especially the host, which move players to a room"
  (:require [com.stuartsierra.component :as component]
            [tenkiwi.room-atoms :as room-atoms]))

(defrecord Register [creds world open-rooms]
  component/Lifecycle
  (start [component]
    (println "Booting Register now...")
    (let [{:keys [rooms]} @(get component :world)]
      ;; Only reboot rooms that are active
      #_(reduce room-atoms/open-room
              component
              (keep :atom-id (vals rooms))))
    component)
  (stop [component]
    (println "Closing down rooms, shutting out players...")
    (let [{:keys [rooms]}  @(get component :world)]
      (reduce room-atoms/close-room
              component
              (keep :atom-id (vals rooms)))
      (-> component :open-rooms (reset! {}))
      component)
    ;; Don't kill the world on reboot
    #_(-> component :world (reset! {}))))

(defn new-register
  ([] (->Register (atom {})))
  ([creds]
   (let [world-atom (room-atoms/load-core creds)
         rooms (atom {})]
     (->Register creds world-atom rooms))))
