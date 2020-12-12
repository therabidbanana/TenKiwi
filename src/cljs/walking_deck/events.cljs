(ns walking-deck.events
  (:require [re-frame.core :as re-frame]
            [walking-deck.db :as db]))

(re-frame/reg-event-db
 :initialize-db
 (fn  [_ _]
   db/default-db))

(re-frame/reg-event-fx
 :initialize-system
 (fn  [_ _]
   {:fx [[:websocket [:user/connected!]]]}))

(re-frame/reg-event-db
 :join/set-params
 (fn [db [_ params]]
   (update-in db [:join] merge params)))

(re-frame/reg-event-db
 :user/room-joined!
 (fn [db [_ params]]
   (update-in db [:user] assoc :current-room params)))

(re-frame/reg-event-db
 :user/booted!
 (fn [db [_ params]]
   (update-in db [:user] assoc :current-room nil)))

(re-frame/reg-event-db
 :room/user-joined!
 (fn [db [_ params]]
   (let [current-room (get-in db [:user :current-room])]
     (if (= (:room-code params) (:room-code current-room))
       (update-in db [:user] assoc :current-room params)))))

(re-frame/reg-event-db
 :room/user-left!
 (fn [db [_ params]]
   (let [current-room (get-in db [:user :current-room])]
     (if (= (:room-code params) (:room-code current-room))
       (update-in db [:user] assoc :current-room params)))))

(re-frame/reg-event-fx
 :join/join-room!
 (fn [{:keys [db]} [_ val]]
   (let [join (:join db)]
     {:fx [[:websocket [:room/join-room! join]]]})))

(re-frame/reg-event-fx
 :room/boot-player!
 (fn [{:keys [db]} [_ val]]
   {:fx [[:websocket [:room/boot-player! val]]]}))
