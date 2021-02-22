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
       "Start FTQ (Original)"]
      [:button {:on-click #(do
                             (dispatch [:->game/start! :ftq {:game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=59533190&single=true&output=tsv"}])
                             (.preventDefault %))}
       "Start FTQ (The Captain)"]
      [:button {:on-click #(do
                             (dispatch [:->game/start! :debrief])
                             (.preventDefault %))}
       "Start Debrief"]
      ]]))

(defn lobby-panel []
  (let [game-data (re-frame/subscribe [:room])]
    [-lobby-panel game-data re-frame/dispatch]))

(defn -debrief-game-panel [user-data dispatch]
  (let [{user-id        :id
         :as            data
         {:as   room
          :keys [game]} :current-room} @user-data
        active?                        (= user-id (:id (:active-player game)))
        {:keys [stage
                all-players
                player-ranks
                player-scores
                company
                players-by-id
                dossiers]}             game

        all-players    (map #(merge % (get dossiers (:id %) {}))
                            all-players)
        voting-active? (if-not (#{:intro} stage)
                         true
                         false)
        display        (if active?
                         (:active-display game)
                         (:inactive-display game))
        x-carded?      (:x-card-active? display)

        self-vote?    (fn [{:keys                   [action params]
                            {:keys [id rank round]} :params
                            :as                     button}]
                        (and (#{:rank-player} action)
                             (= user-id id)))
        valid-button? (fn [{:keys                   [action params disabled?]
                            {:keys [id rank round]} :params
                            :as                     button}]
                        (cond
                          (#{:rank-player} action)
                          (and
                           (not= user-id id)
                           (nil? (get-in player-ranks [user-id round rank]))
                           (not= id (get-in player-ranks [user-id round :best])))
                          :else
                          (not disabled?)))
        ]
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
        (map (fn [{:keys [name value label generator]}]
               (with-meta
                 [:div.user-input
                  [:label label]
                  [:br]
                  [:input {:name name :value value}]]
                 {:key name}))
             (get-in display [:card :inputs]))]
         [:div.actions
          (map
           (fn [{:keys    [action text params]
                 confirm? :confirm
                 :or      {params {}}
                 :as      button}]
             (with-meta
               [:div.action {:class    (str (if-not (valid-button? button) " disabled")
                                            (if (self-vote? button) " hidden"))
                             :on-click #(if (and
                                             (valid-button? button)
                                             (or (not confirm?) (js/confirm "Are you sure?")))
                                          (dispatch [:->game/action! action params])) }
                [:a {} text]]
               {:key (str action params)}))
           (get-in display [:actions]))]]
      ]
     [:div.extras
      [:div.company
       [:h2 (str (:name company) " Values:")]
       [:ul
        (map
         (fn [val] (with-meta [:li val] {:key val}))
         (:values company))]]
      (if voting-active?
        (map (fn [{:keys [id user-name dead? agent-name agent-codename agent-role]}]
               (let [total-score (apply + (vals (player-scores id)))]
                 (with-meta
                   [:div.player
                    [:div.player-name
                     {:title (if agent-name
                               (str "Real Name: " agent-name))}
                     (str "[ " total-score " ] " (if agent-name (str agent-codename ", " agent-role " ")) " (" user-name ")")]
                    [:div.score-actions
                     ;; TODO - maybe this logic should come from gamemaster
                     (if-not (= id user-id)
                       [:a.downvote-player {:on-click #(dispatch [:->game/action! :downvote-player {:player-id id}])} " - "])
                     (str (get-in player-scores [id user-id]))
                     (if-not (= id user-id)
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
