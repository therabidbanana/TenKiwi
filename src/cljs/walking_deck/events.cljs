(ns walking-deck.events
  (:require [re-frame.core :as re-frame]
            [walking-deck.db :as db]))

(re-frame/reg-event-db
 :initialize-db
 (fn  [_ _]
   db/default-db))
