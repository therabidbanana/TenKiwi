(ns walking-deck.core
  (:require [reagent.dom :as reagent.dom]
            [re-frame.core :as re-frame]
            [walking-deck.events]
            [walking-deck.subs]
            [walking-deck.views :as views]
            [walking-deck.config :as config]))

(enable-console-print!)

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent.dom/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn render []
  (re-frame/dispatch-sync [:initialize-db])
  (dev-setup)
  (mount-root))
