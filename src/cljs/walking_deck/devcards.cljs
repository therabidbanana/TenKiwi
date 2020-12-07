(ns walking-deck.devcards
  (:require
   [reagent.core :as reagent]) ; just for example
  (:require-macros
   [devcards.core :refer [defcard]]))

(defcard reagent-no-help
  (reagent/as-element [:h1 "Reagent example"]))
