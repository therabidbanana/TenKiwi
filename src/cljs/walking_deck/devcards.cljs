(ns walking-deck.devcards
  (:require
   [reagent.core :as reagent]
   [walking-deck.views :as views]
   [devcards.core :refer-macros [defcard defcard-rg]]) ; just for example
  )

(defcard reagent-no-help
  (reagent/as-element [:h1 "Reagent example"]))

(defcard-rg main-panel
  [views/-main-panel "foo"] ;; <-- 1
  #_(atom "foo") ;; <-- 2
  #_{:inspect-data true} ;; <-- 3
  )

(def user-state (reagent/atom {:user-name "foobar"}))

(defmulti dispatch (fn [[event & args]] event))

(defmethod dispatch :set-user-name [[_ name]]
  (swap! user-state #(assoc % :user-name name)))

(defcard-rg join-panel
  [views/-join-panel user-state dispatch] ;; <-- 1
  user-state ;; <-- 2
  {:inspect-data true} ;; <-- 3
  )
