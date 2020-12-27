(ns tenkiwi.components.timekeeper
  "Timekeeper updates the clock and triggers any updates to sente"
  (:require [com.stuartsierra.component :as component]
            [clojure.core.async :refer [chan go-loop alt! timeout put!]]))

(defrecord Timekeeper [tick-size tick-fn]
  component/Lifecycle
  (start [component]
    (let [shutdown-ch (chan)]
      (println "Starting clock...")
      (go-loop []
        (alt!
          (timeout tick-size) (do
                                (tick-fn (:sente component))
                                (recur))
          shutdown-ch nil))
      (assoc component :shutdown! shutdown-ch)))
  (stop [component]
    (println "Cancelling clock")
    (put! (:shutdown! component) true)
    (dissoc component :shutdown!)))

(defn new-timekeeper
  "tick-fn should be a function prepared to take the sente component (such as gm/host)"
  ([tick-fn] (->Timekeeper 1000 tick-fn))
  ([tick tick-fn] (->Timekeeper tick tick-fn)))
