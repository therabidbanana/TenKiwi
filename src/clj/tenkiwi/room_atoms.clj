(ns tenkiwi.room-atoms
  "Used for keeping track of gamestate across all players. The register is
  updated by gamemasters, especially the host, which move players to a room"
  (:require [amazonica.aws.s3 :as aws]
            [taoensso.nippy :as nippy]
            [duratom.duratom.core :refer [duratom]]
            [duratom.duratom.utils :refer [s3-bucket-bytes]]))


(defn- load-atom [creds atom-key]
  (duratom
   :aws-s3
   :bucket (get creds :bucketname "tenkiwi-test")
   :key atom-key
   :credentials creds
   :init {}
   :rw {:read  (comp nippy/thaw s3-bucket-bytes)
        :write nippy/freeze}))

(defn- kill-atom [creds atom-key]
  (println "Destroying room at " atom-key)
  (aws/delete-object
   creds
   :bucket-name (:bucketname creds)
   :key atom-key))

(defn load-core [creds]
  (load-atom creds "0"))

(defn open-room [register room-id]
  (let [open-rooms (:open-rooms register)]
    (if (get @open-rooms room-id)
      register
      (do
        (println "Opening room at " room-id)
        (swap! open-rooms assoc room-id (load-atom (:creds register) room-id))
        register))))

(defn close-room
  ([register room-id delete!]
   (let [room-atom (-> register :open-rooms deref (get room-id))
         register  (close-room register room-id)]
     (if delete!
       (try
         (do
           (duratom.duratom.core/destroy room-atom)
           (kill-atom (:creds register) room-id))
         (catch Exception e
           (println e))))
     register))
  ([register room-id]
   (let [open-rooms (:open-rooms register)]
     (if (get @open-rooms room-id)
       (do
         (println "Closing room at " room-id)
         (swap! open-rooms dissoc room-id)
         register)
       register))))
