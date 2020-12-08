(ns walking-deck.views
  (:require [re-frame.core :as re-frame]))

(defn -join-panel [user dispatch]
  (let [val #(-> % .-target .-value)]
    [:div {:class "form"}
     [:form
      [:div.fieldset
       [:label
        "Name"
        [:br]
        [:input {:name      "name"
                 :value     (-> user deref :user-name)
                 :on-change #(dispatch [:set-user-name (val %)])}]]]
      [:div.fieldset
       [:label
        "Room"
        [:br]
        [:input {:name "room"}]]]
      [:button "Join"]]]))

(defn join-panel []
  (let [user-atom   (re-frame/subscribe [:user])
        new-allowed true]
    [-join-panel user-atom re-frame/dispatch]))

(defn -main-panel [name]
  (let []
    [:div "Hello from " name]))

(defn main-panel []
  (let [name (re-frame/subscribe [:name])]
    [:div {}
     [join-panel]
     [-main-panel @name]]))
