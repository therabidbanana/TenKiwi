(ns walking-deck.devcards
  (:require
   [reagent.core :as reagent]
   [devcards.core :refer-macros [defcard]]) ; just for example
  )

(defcard reagent-no-help
  (reagent/as-element [:h1 "Reagent example"]))
