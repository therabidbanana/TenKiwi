(ns walking-deck.styles
  (:require [garden-watcher.def :refer [defstyles]]))

(def base
  (list
   [:h1 {:text-decoration "underline"}]
   [:input {:width "100%"
            :font-size "2rem"
            :margin "0.5rem"}]
   [:form {:max-width "80%"
           :margin "0 auto"
           :padding "1rem"}]
   [:button {:font-size "2rem"}]
   [:body {:color "#eeeef0"
           :font-size "1.4em"
           :font-family "Tahoma"
           :background-color "#223"}]))

(def lobby
  (list
   [:.lobby
    {:max-width "80%"
     :margin "0 auto"
     }

    [:li {:border "1px solid #bbb"
          :font-size "2rem"
          :list-style 'none
          :padding "0.6rem"}]
    [:.boot {:cursor 'pointer
             :padding "0.6rem 1rem"
             :margin-top "-0.6rem"
             :margin-right "-0.6rem"
             :overflow "hidden"
             :color "white"
             :background "red"
             :float 'right}]]))

(defstyles style
  (conj [] base lobby))
