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

(re-frame/reg-event-fx
 :user/connected!
 [(re-frame/inject-cofx :system :client-id)]
 (fn [{:as x :keys [db client-id]} [_ params]]
   (println x)
   {:db (update-in db [:user] assoc
                   :id client-id
                   :connected? true)}))

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

(re-frame/reg-event-fx
 :->game/action!
 (fn [db [_ action]]
   (let [room-id (get-in db [:user :current-room :id])]
     {:fx [[:websocket [:game/action! {:action  action
                                       :room-id room-id}]]]})))

(re-frame/reg-event-db
 :game/changed!
 (fn [db [_ params]]
   (let [current-room (get-in db [:user :current-room])]
     (if (= (:room-code params) (:room-code current-room))
       (do
         (println params)
         (update-in db [:user] assoc :current-room params))))))

(re-frame/reg-event-db
 :game/started!
 (fn [db [_ params]]
   (let [current-room (get-in db [:user :current-room])]
     (if (= (:room-code params) (:room-code current-room))
       (do
         (println params)
         (update-in db [:user] assoc :current-room params))))))

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
 :->game/start!
 (fn [{:keys [db]} [_ id]]
   {:fx [[:websocket [:game/start! id]]]}))

(re-frame/reg-event-fx
 :->join/join-room!
 (fn [{:keys [db]} [_ val]]
   (let [join (:join db)]
     {:fx [[:websocket [:room/join-room! join]]]})))

(re-frame/reg-event-fx
 :->room/boot-player!
 (fn [{:keys [db]} [_ val]]
   {:fx [[:websocket [:room/boot-player! val]]]}))
