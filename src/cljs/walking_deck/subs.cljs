(ns walking-deck.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub
 :name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 :user
 (fn [db]
   (:user db)))

(re-frame/reg-sub
 :room
 (fn [db]
   (get-in db [:user :current-room])))

(re-frame/reg-sub
 :game
 (fn [db]
   (get-in db [:user :current-room :game])))

(re-frame/reg-sub
 :join
 (fn [db]
   (:join db)))
