(ns walking-deck.components.register
  "Used for keeping track of gamestate across all players. The register is
  updated by gamemasters, especially the host, which move players to a room"
  (:require [com.stuartsierra.component :as component]))

(defrecord Register [world]
  component/Lifecycle
  (start [component]
    (println "Booting Register...")
    component)
  (stop [component]
    (println "Closing down rooms, shutting out players...")
    (-> component :world (reset! {}))
    component))

(defn new-register
  ([] (->Register (atom {})))
  ([world-atom] (->Register world-atom)))
