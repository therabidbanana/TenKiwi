(ns walking-deck.application
  (:gen-class)
  (:require [com.stuartsierra.component :as component]
            [walking-deck.components.server-info :refer [server-info]]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [system.components.endpoint :refer [new-endpoint]]
            [system.components.handler :refer [new-handler]]
            [system.components.middleware :refer [new-middleware]]
            [system.components.http-kit :refer [new-web-server]]
            [system.components.sente :refer [new-channel-socket-server sente-routes]]
            [walking-deck.socket-events :refer [event-msg-handler]]
            [walking-deck.config :refer [config]]
            [walking-deck.components.gamemaster :refer [new-gamemaster]]
            [walking-deck.routes :refer [home-routes]]))

(defn app-system [config]
  (component/system-map
   :routes     (new-endpoint home-routes)
   :sente-endpoint (component/using
                    (new-endpoint sente-routes)
                    [:sente])
   :middleware (new-middleware {:middleware (:middleware config)})

   :handler    (-> (new-handler)
                   (component/using [:sente-endpoint :routes :middleware]))
   :http       (-> (new-web-server (:http-port config))
                   (component/using [:handler]))
   :sente (component/using
           (new-channel-socket-server
            event-msg-handler
            (get-sch-adapter)
            {:user-id-fn      (fn [ring-req] (:client-id ring-req))
             :wrap-component? true})
           [:gamemaster])
   :gamemaster  (new-gamemaster)
   :server-info (server-info (:http-port config))))

(defn -main [& _]
  (let [config (config)]
    (-> config
        app-system
        component/start)))
