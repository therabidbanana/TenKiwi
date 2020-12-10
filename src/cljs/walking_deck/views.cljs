(ns walking-deck.views
  (:require [re-frame.core :as re-frame]))

(defn -join-panel [join dispatch]
  (let [val #(-> % .-target .-value)]
    [:div {:class "form"}
     [:form
      [:div.fieldset
       [:label
        "Name"
        [:br]
        [:input {:name      "name"
                 :value     (-> join deref :user-name)
                 :on-change #(dispatch [:join/set-params {:user-name (val %)}])}]]]
      [:div.fieldset
       [:label
        "Lobby Code"
        [:br]
        [:input {:name "lobby-code"
                 :value (-> join deref :room-code)
                 :on-change #(dispatch [:join/set-params {:room-code (val %)}])}]]]
      [:button {:on-click #(do
                             (dispatch [:join/join-room!])
                             (.preventDefault %))}
       "Join"]]]))

(defn join-panel []
  (let [user-atom   (re-frame/subscribe [:join])
        new-allowed true]
    [-join-panel user-atom re-frame/dispatch]))

(defn -lobby-panel [game-data dispatch]
  (let [game-data @game-data]
    [:div.lobby
     [:ul.players
      (for [player (:players game-data)]
        ^{:key (:id player)} [:li (:user-name player)
                              [:a {:on-click #(dispatch [:room/boot-player (:id player)])} "x"]])]]))

(defn lobby-panel []
  (let [game-data (re-frame/subscribe [:room])]
    [-lobby-panel game-data re-frame/dispatch]))

(defn -main-panel [name]
  (let []
    [:div "Hello from " name]))

(defn main-panel []
  (let [user (re-frame/subscribe [:user])
        name (re-frame/subscribe [:name])]
    [:div {}
     (if (get @user :current-room)
       [lobby-panel]
       [join-panel])
     [-main-panel @name]]))
