(ns walking-deck.styles
  (:require [garden-watcher.def :refer [defstyles]]))

(defstyles style
  [:h1 {:text-decoration "underline"}]
  [:input {:width "100%"
           :font-size "2rem"
           :margin "1rem"}]
  [:form {:width "90%"
          :margin "0 auto"}]
  [:body {:color "#eeeef0"
          :font-size "1.4em"
          :background-color "#223"}])
