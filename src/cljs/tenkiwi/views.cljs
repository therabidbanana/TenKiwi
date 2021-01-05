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
      ;; Easter Egg - For The Queen
      (if (= (:room-code game-data) "haslem")
        [:button {:on-click #(do
                               (dispatch [:->game/start! :ftq])
                               (.preventDefault %))}
         "Start FTQ (Original)"])
      (if (= (:room-code game-data) "haslem")
        [:button {:on-click #(do
                               (dispatch [:->game/start! :ftq {:game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=59533190&single=true&output=tsv"}])
                               (.preventDefault %))}
         "Start FTQ (The Captain)"])
      (if (= (:room-code game-data) "haslem")
        [:button {:on-click #(do
                               (dispatch [:->game/start! :debrief])
                               (.preventDefault %))}
         "Start Debrief"])
      [:button {:on-click #(do
                             (dispatch [:->game/start! :walking-deck {:extra-players 0}])
                             (.preventDefault %))}
       "Start Walking Deck + 0"]
      [:button {:on-click #(do
                             (dispatch [:->game/start! :walking-deck {:extra-players 1}])
                             (.preventDefault %))}
       "Start Walking Deck + 1"]
      [:button {:on-click #(do
                             (dispatch [:->game/start! :walking-deck {:extra-players 2}])
                             (.preventDefault %))}
       "Start Walking Deck + 2"]
      [:button {:on-click #(do
                             (dispatch [:->game/start! :walking-deck {:extra-players 3}])
                             (.preventDefault %))}
       "Start Walking Deck + 3"]
      ]]))

(defn lobby-panel []
  (let [game-data (re-frame/subscribe [:room])]
    [-lobby-panel game-data re-frame/dispatch]))

(defn -walking-deck-game-panel [user-data dispatch]
  (let [{user-id        :id
         :as            data
         {:as   room
          :keys [game]} :current-room} @user-data
        active?                        (= user-id (:id (:active-player game)))
        {:keys [act
                act-prompt
                act-timer
                drama-timer
                next-players
                active-player]}        game
        players                        (cons (assoc active-player :active? true)
                                             next-players)
        display                        (if active?
                                         (:active-display game)
                                         (:inactive-display game))
        x-carded?                      (:x-card-active? display)]
    [:div.game-table
     [:div.current {}
      [:div.timer {} (if (> act 3)
                       (str "End game")
                       (str "Act:" act
                           " " act-prompt
                           ;; " Time left: " act-timer " seconds"
                           ))]
      [:div.active-area {}
       [:div.x-card {:class (if x-carded? "active" "inactive")}
        [:a {:on-click #(dispatch [:->game/action! :x-card])} "X"]]
       [:div.card {:class (str " "
                               (if x-carded?
                                 "x-carded"))}
          (-> (get-in display [:card :text])
              (m/md->hiccup)
              (m/component))]
         [:div.actions
          (map-indexed
           (fn [idx {:keys [action text]}] (with-meta (vector :div.action [:a {:on-click #(dispatch [:->game/action! action])} text]) {:key idx}))
               (get-in display [:actions]))]]
      ]
     [:div.extras
      (map-indexed (fn [idx {:keys [active? id user-name dead? character]}]
             (with-meta [:div.player {:class (str (if active? "active"))
                                      :title (:description character)}
                         (str
                          (if active? "* ")
                          (:title character) " (" user-name ")"
                              (if dead? "- dead"))] {:key idx}))
           players)
      (map-indexed (fn [idx
                        {conf  :confirm :keys [action class text]}]
             (with-meta (vector :div.extra-action {:class class} [:a.button {:on-click #(if (or (not conf) (js/confirm "Are you sure?"))
                                                                                          (dispatch [:->game/action! action]))} text]) {:key idx}))
           (get-in display [:extra-actions]))]]))

(defn -debrief-game-panel [user-data dispatch]
  (let [{user-id        :id
         :as            data
         {:as   room
          :keys [game]} :current-room} @user-data
        active?                        (= user-id (:id (:active-player game)))
        {:keys [stage
                all-players
                player-scores
                players-by-id]}        game
        voting-active?                 (if-not (#{:intro} stage)
                                         true
                                         false)
        display                        (if active?
                                         (:active-display game)
                                         (:inactive-display game))
        x-carded?                      (:x-card-active? display)]
    [:div.game-table
     [:div.current {}
      [:div.active-area {}
       [:div.x-card {:class (if x-carded? "active" "inactive")}
        [:a {:on-click #(dispatch [:->game/action! :x-card])} "X"]]
       [:div.card {:class (str " "
                               (if x-carded?
                                 "x-carded"))}
          (-> (get-in display [:card :text])
              (m/md->hiccup)
              (m/component))
        (map (fn [{:keys [name value label]}]
               (with-meta
                 [:input {:name name :value value}]
                 {:id name}))
             (get-in display [:card :inputs]))]
         [:div.actions
          (map (fn [{:keys [action text]}] (with-meta (vector :div.action [:a {:on-click #(dispatch [:->game/action! action])} text]) {:key action}))
               (get-in display [:actions]))]]
      ]
     [:div.extras
      ;; TODO : allow character names inline
      (if voting-active?
        (map (fn [{:keys [id user-name dead?]}]
               (let [total-score (apply + (vals (player-scores id)))]
                 (with-meta
                   [:div.player
                    [:div.player-name
                     (str "[ " total-score " ] " user-name)]
                    [:div.score-actions
                     ;; TODO - maybe this logic should come from gamemaster
                     (if-not (or active? (= id user-id))
                       [:a.downvote-player {:on-click #(dispatch [:->game/action! :downvote-player {:player-id id}])} " - "])
                     (str (get-in player-scores [id user-id]))
                     (if-not (or active? (= id user-id))
                       [:a.upvote-player {:on-click #(dispatch [:->game/action! :upvote-player {:player-id id}])} " + "])
                     ]]
                   {:key id})))
             all-players))
      (map (fn [{conf  :confirm
                 :keys [action class text]}]
             (with-meta (vector :div.extra-action {:class class} [:a.button {:on-click #(if (or (not conf) (js/confirm "Are you sure?"))
                                                                                          (dispatch [:->game/action! action]))} text]) {:key action}))
           (get-in display [:extra-actions]))]]))

(defn -ftq-game-panel [user-data dispatch]
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
      [:img {:src (str (:text queen))}]
      (map (fn [{conf :confirm
                 :keys [action class text]}]
             (with-meta (vector :div.extra-action {:class class} [:a.button {:on-click #(if (or (not conf) (js/confirm "Are you sure?"))
                                                                                          (dispatch [:->game/action! action]))} text]) {:key action}))
           (get-in display [:extra-actions]))]]))

(defn game-panel []
  (let [user-data (re-frame/subscribe [:user])
        room (re-frame/subscribe [:room])
        game-type (get-in @user-data [:current-room :game :game-type])]
    (case game-type
      :ftq
      [-ftq-game-panel user-data re-frame/dispatch]
      :walking-deck
      [-walking-deck-game-panel user-data re-frame/dispatch]
      :debrief
      [-debrief-game-panel user-data re-frame/dispatch]
      )))

(defn -connecting-panel []
  (let []
    [:div "Connecting to server..."]))

(defn main-panel []
  (let [user (re-frame/subscribe [:user])
        room (re-frame/subscribe [:room])
        game (get-in @user [:current-room :game :game-type])]
    [:div {}
     (cond
       game [game-panel]
       (get @user :current-room) [lobby-panel]
       (get @user :connected?) [join-panel]
       :else [-connecting-panel])
     ]))
