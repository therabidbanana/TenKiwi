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

(def user-state (atom {:user-name "foobar"}))

(defcard-rg login-panel
  [views/-login-panel user-state] ;; <-- 1
  user-state ;; <-- 2
  {:inspect-data true} ;; <-- 3
  )
