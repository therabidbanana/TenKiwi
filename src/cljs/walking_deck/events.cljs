(ns walking-deck.events
  (:require [re-frame.core :as re-frame]
            [walking-deck.db :as db]))

(re-frame/reg-event-db
 :initialize-db
 (fn  [_ _]
   db/default-db))

(re-frame/reg-event-db
 :join/set-params
 (fn [db [_ params]]
   (update-in db [:join] merge params)))

(re-frame/reg-event-db
 :user/room-joined!
 (fn [db [_ params]]
   (update-in db [:user] assoc :current-room params)))

(re-frame/reg-event-fx
 :join/join-room!
 (fn [{:keys [db]} [_ val]]
   (let [join (:join db)]
     {:fx [[:websocket [:room/join-room! join]]]})))
