(ns walking-deck.components.gamemaster
  (:require [com.stuartsierra.component :as component]))

(defrecord Gamemaster [room-atom player-atom]
  component/Lifecycle
  (start [component]
    (println "Booting gamemaster...")
    (assoc component :rooms room-atom :players player-atom))
  (stop [component]
    (println "Closing down rooms, shutting out players...")
    (-> component :rooms (reset! {}))
    (-> component :players (reset! {}))
    component))

(defn new-gamemaster
  ([] (->Gamemaster (atom {}) (atom {})))
  ([room-atom player-atom] (->Gamemaster room-atom player-atom)))
