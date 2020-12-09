(ns walking-deck.system
  (:require [com.stuartsierra.component :as component]
            [walking-deck.socket-events :refer [chsk]]
            [walking-deck.components.ui :refer [new-ui-component]]))

(declare system)

;; TODO: CHSK should probably live in here (prevent CSRF failures on figwheel?)
(defn new-system []
  (component/system-map
   :app-root (new-ui-component)))

(defn init []
  (set! system (new-system)))

(defn start []
  (set! system (component/start system)))

(defn stop []
  (set! system (component/stop system)))

(defn ^:export go []
  (init)
  (start))

(defn reset []
  (stop)
  (go))
