(ns tenkiwi.components.register
  "Used for keeping track of gamestate across all players. The register is
  updated by gamemasters, especially the host, which move players to a room"
  (:require [com.stuartsierra.component :as component]
            [taoensso.nippy :as nippy]
            [duratom.core :refer [duratom]]))

(defrecord Register [world]
  component/Lifecycle
  (start [component]
    (println "Booting Register...")
    component)
  (stop [component]
    (println "Closing down rooms, shutting out players...")
    component
    ;; Don't kill the world on reboot
    #_(-> component :world (reset! {}))))

(defn new-register
  ([] (->Register (atom {})))
  ([connection-uri]
   (let [world-atom (duratom
                     :postgres-db
                     :table-name "atom_state"
                     :row-id 0
                     :init {}
                     :rw {:read nippy/thaw
                          :write nippy/freeze
                          :column-type :bytea}
                     :db-config
                     (str connection-uri))]
     (->Register world-atom))))
