(ns tenkiwi.views
  (:require [re-frame.core :as re-frame]
            [markdown-to-hiccup.core :as m]))

(defn -join-panel [join dispatch]
  (let [val #(-> % .-target .-value)]
    [:div {:class "form"}
     [:form
      [:div.fieldset
       [:label
        "Name"
        [:br]
        [:input {:name      "game-user-name"
                 :value     (-> join deref :user-name)
                 :on-change #(dispatch [:join/set-params {:user-name (val %)}])}]]]
      [:div.fieldset
       [:label
        "Lobby Code"
        [:br]
        [:input {:name      "game-lobby-code"
                 :value     (-> join deref :room-code)
                 :on-change #(dispatch [:join/set-params {:room-code (val %)}])}]]]
      [:button {:on-click #(do
                             (dispatch [:->join/join-room!])
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
        ^{:key (:id player)}
        [:li (:user-name player)
         [:a.boot {:on-click #(dispatch [:->room/boot-player! (:id player)])} "x"]])]
     [:div.actions
      [:button {:on-click #(do
                             (dispatch [:->game/start! :ftq])
                             (.preventDefault %))}
       "Start FTQ"]]]))

(defn lobby-panel []
  (let [game-data (re-frame/subscribe [:room])]
    [-lobby-panel game-data re-frame/dispatch]))

(defn -game-panel [user-data dispatch]
  (let [{user-id        :id
         :as            data
         {:as   room
          :keys [game]} :current-room} @user-data
        active?                        (= user-id (:id (:active-player game)))
        queen                          (:queen game)
        display                        (if active?
                                         (:active-display game)
                                         (:inactive-display game))
        x-carded?                      (:x-card-active? display)]
    [:div.game-table
     [:div.current {}
      [:div.active-area {}
       [:div.x-card {:class (if x-carded? "active" "inactive")}
        [:a {:on-click #(dispatch [:->game/action! :x-card])} "X"]]
       [:div.card {:class (str (name (get-in display [:card :state]))
                               " "
                               (if x-carded?
                                 "x-carded"))}
          (-> (get-in display [:card :text])
              (m/md->hiccup)
              (m/component))]
         [:div.actions
          (map (fn [{:keys [action text]}] (with-meta (vector :div.action [:a {:on-click #(dispatch [:->game/action! action])} text]) {:key action}))
               (get-in display [:actions]))]]
      ]
     [:div.extras
      [:img {:src (str "/" queen)}]
      (map (fn [{conf :confirm
                 :keys [action class text]}]
             (with-meta (vector :div.extra-action {:class class} [:a.button {:on-click #(if (or (not conf) (js/confirm "Are you sure?"))
                                                                                          (dispatch [:->game/action! action]))} text]) {:key action}))
           (get-in display [:extra-actions]))]]))

(defn game-panel []
  (let [user-data (re-frame/subscribe [:user])
        room (re-frame/subscribe [:room])]
    [-game-panel user-data re-frame/dispatch]))

(defn -connecting-panel []
  (let []
    [:div "Connecting to server..."]))

;; TODO - doesn't rerender?
(defn main-panel []
  (let [user (re-frame/subscribe [:user])
        room (re-frame/subscribe [:room])
        game (get-in @user [:current-room :game])]
    [:div {}
     (cond
       game [game-panel]
       (get @user :current-room) [lobby-panel]
       (get @user :connected?) [join-panel]
       :else [-connecting-panel])
     ]))
