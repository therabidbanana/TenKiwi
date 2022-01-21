(ns tenkiwi.application
  (:gen-class)
  (:require [com.stuartsierra.component :as component]
            [tenkiwi.components.server-info :refer [server-info]]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [taoensso.sente.packers.transit :refer [get-transit-packer]]
            [system.components.endpoint :refer [new-endpoint]]
            [system.components.handler :refer [new-handler]]
            [system.components.middleware :refer [new-middleware]]
            [system.components.http-kit :refer [new-web-server]]
            [system.components.sente :refer [sente-routes]]
            [tenkiwi.socket-events :refer [event-msg-handler tick-fn]]
            [tenkiwi.config :refer [config]]
            [tenkiwi.components.register :refer [new-register]]
            [tenkiwi.components.sente :refer [new-channel-socket-server]]
            [tenkiwi.components.timekeeper :refer [new-timekeeper]]
            [tenkiwi.routes :refer [home-routes]]))

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
   :clock     (component/using (new-timekeeper tick-fn)
                               [:sente :register])
   :sente (component/using
           (new-channel-socket-server
            event-msg-handler
            (get-sch-adapter)
            {:user-id-fn      (fn [ring-req] (:client-id ring-req))
             ;; In general all sessions are considered unauthenticated
             ;; so CSRF is not likely to be a strong attack vector
             :csrf-token-fn nil
             :packer (get-transit-packer)
             :wrap-component? true})
           [:register])
   :register  (new-register (:s3-creds config))
   :server-info (server-info (:http-port config))))

(defn -main [& _]
  (let [config (config)]
    (-> config
        app-system
        component/start)))
