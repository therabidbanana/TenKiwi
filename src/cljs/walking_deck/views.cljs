(ns walking-deck.views
  (:require [re-frame.core :as re-frame]))

(defn -login-panel [user]
  (let [val #(-> % .-target .-value)]
    [:div {:class "form"}
     [:form
      [:input {:name      "name"
               :value     (-> user deref :user-name)
               :on-change (fn [e] (swap! user #(assoc % :user-name (val e))))}]
      [:input {:name "room"}]]]))

(defn login-panel []
  (let [user-atom   (re-frame/subscribe [:user])
        new-allowed true]
    [-login-panel user-atom]))

(defn -main-panel [name]
  (let []
    [:div "Hello from " name]))

(defn main-panel []
  (let [name (re-frame/subscribe [:name])]
    [:div {}
     [login-panel]
     [-main-panel @name]]))
