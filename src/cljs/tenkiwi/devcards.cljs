(ns tenkiwi.devcards
  (:require
   [reagent.core :as reagent]
   [tenkiwi.views :as views]
   [devcards.core :refer-macros [defcard defcard-rg]]) ; just for example
  )

(defcard reagent-no-help
  (reagent/as-element [:h1 "Reagent example"]))

(defcard-rg main-panel
  [views/-main-panel "foo"] ;; <-- 1
  #_(atom "foo") ;; <-- 2
  #_{:inspect-data true} ;; <-- 3
  )

(def user-state (reagent/atom {:user-name "foobar"
                               :room-code "xr3b"}))

(def game-state (reagent/atom {:room-code "xr3b"
                               :players   [{:id        (random-uuid)
                                            :user-name "Bonni"
                                            :room-code "xr3b"}
                                           {:id        "5"
                                            :user-name "David"}]}))

(defmulti dispatch (fn [[event & args]] (do (println "Event= " event args) event)))

(defmethod dispatch :join/set-params [[_ params]]
  (swap! user-state #(merge % params)))

(defmethod dispatch :join/set-room-code [[_ name]]
  (swap! user-state #(assoc % :room-code name)))

(defmethod dispatch :room/boot-player [[_ player-id]]
  (let [booted! #(= (:id %) player-id)]
    (swap! game-state #(update-in % [:players] (partial remove booted!)))))

(defcard-rg join-panel
  [views/-join-panel user-state dispatch] ;; <-- 1
  user-state ;; <-- 2
  {:inspect-data true} ;; <-- 3
  )

(defcard-rg lobby-panel
  [views/-lobby-panel game-state dispatch] ;; <-- 1
  game-state ;; <-- 2
  {:inspect-data true} ;; <-- 3
  )
