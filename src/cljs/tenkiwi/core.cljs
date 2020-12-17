(ns tenkiwi.core
  (:require [reagent.dom :as reagent.dom]
            [re-frame.core :as re-frame]
            [tenkiwi.events]
            [tenkiwi.subs]
            [tenkiwi.views :as views]
            [tenkiwi.config :as config]))

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
