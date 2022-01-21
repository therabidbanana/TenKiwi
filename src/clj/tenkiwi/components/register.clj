(ns tenkiwi.components.register
  "Used for keeping track of gamestate across all players. The register is
  updated by gamemasters, especially the host, which move players to a room"
  (:require [com.stuartsierra.component :as component]
            [amazonica.core :refer [with-credential defcredential]]
            [amazonica.aws.s3 :as aws]
            [taoensso.nippy :as nippy]
            [duratom.duratom.core :refer [duratom]]
            [duratom.duratom.utils :refer [s3-bucket-bytes]]))

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
  ([creds]
   (let [world-atom (duratom
                     :aws-s3
                     :bucket (get creds :bucketname "tenkiwi-test")
                     :key "0"
                     :credentials creds
                     :init {}
                     :rw {:read  (comp nippy/thaw s3-bucket-bytes)
                          :write nippy/freeze}
                     #_:db-config
                     #_(str connection-uri)
                     )]
     (->Register world-atom))))
