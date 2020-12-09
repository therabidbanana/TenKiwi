(ns walking-deck.events
  (:require [re-frame.core :as re-frame]
            [walking-deck.socket-events :as socket-events]
            [walking-deck.db :as db]))

(re-frame/reg-event-db
 :initialize-db
 (fn  [_ _]
   db/default-db))

(re-frame/reg-event-db
 :join/set-params
 (fn [db [_ params]]
   (update-in db [:join] merge params)))

(re-frame/reg-event-fx
 :join/join-room!
 (fn [{:keys [db]} [_ val]]
   (let [join (:join db)]
     {:fx [[:websocket [:room/join-room! join]]]})))

;; (re-frame/reg-cofx
;;  :now
;;  (fn [coeffects _]
;;    (assoc coeffects :now (js/Date. 2016 1 1))))  ;; then is `:now`

(re-frame/reg-fx
 :websocket
 (fn [chsk-args]
   (println chsk-args)
   ;; TODO: Add timeout, callback for response -> dispatch
   (socket-events/chsk-send! chsk-args)))
