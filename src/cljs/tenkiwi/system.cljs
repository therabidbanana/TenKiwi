(ns tenkiwi.system
  (:require [com.stuartsierra.component :as component]
            [re-frame.core :as re-frame]
            [tenkiwi.socket-events :refer [event-msg-handler]]
            [taoensso.sente.packers.transit :refer [get-transit-packer]]
            [tenkiwi.components.sente :refer [new-channel-socket-client]]
            [tenkiwi.components.ui :refer [new-ui-component]]))

(declare system)

;;;; Scrape document for a csrf token to boot sente with
(def ?csrf-token
  (when-let [el (.getElementById js/document "sente-csrf-token")]
    (.getAttribute el "data-csrf-token")))

(defn ?client-id []
  (let [client-id (js->clj (.getItem js/localStorage "device-id"))
        client-id (or client-id (str (random-uuid)))
        as-str    (clj->js client-id)]
    (do
      (.setItem js/localStorage "device-id" as-str)
      client-id)))

;; TODO: CHSK should probably live in here (prevent CSRF failures on figwheel?)
(defn new-system []
  (component/system-map
   :sente-handler {:handler event-msg-handler}
   :sente (component/using
           (new-channel-socket-client "/chsk" ?csrf-token {:type      :auto
                                                           :packer    (get-transit-packer)
                                                           :client-id (?client-id)})
           [:sente-handler])
   :client-id (?client-id)
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

;; TODO: can I move this?
(re-frame/reg-cofx
 :system
 (fn [cofx component]
   (println component)
   (assoc cofx component (get-in system [component]))))

(re-frame/reg-fx
 :websocket
 (fn [chsk-args]
   (let [chsk-send! (get-in system [:sente :chsk-send!])]
     ;; TODO: Add timeout, callback for response -> dispatch
     (chsk-send! chsk-args))))
