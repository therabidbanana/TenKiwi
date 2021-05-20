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
                             (dispatch [:<-join/join-room!])
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
         [:a.boot {:on-click #(dispatch [:<-room/boot-player! (:id player)])} "x"]])]
     [:div.actions
      (if (= (:room-code game-data) "haslem")
        [:button {:on-click #(do
                               (dispatch [:<-game/start! :ftq])
                               (.preventDefault %))}
         "Start: FTQ (Original)"])
      [:button {:on-click #(do
                             (dispatch [:<-game/start! :ftq {:game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=59533190&single=true&output=tsv"}])
                             (.preventDefault %))}
       "Start: For The Captain"]
      [:button {:on-click #(do
                             (dispatch [:<-game/start! :debrief {:game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=1113383423&single=true&output=tsv"}])
                             (.preventDefault %))}
       "Start: The Debrief"]
      [:button {:on-click #(do
                             (dispatch [:<-game/start! :debrief {:game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=599053556&single=true&output=tsv"}])
                             (.preventDefault %))}
       "Start: The Culinary Contest"]
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
                stage-name
                stage-focus
                all-players
                player-ranks
                player-scores
                company
                players-by-id
                mission
                dossiers]}             game

        all-players    (map #(merge % (get dossiers (:id %) {}))
                            all-players)
        voting-active? (if-not (#{:intro} stage)
                         true
                         false)

        {:keys [extra-details]
         :as   display} (if active?
                           (:active-display game)
                           (:inactive-display game))
        x-carded?       (:x-card-active? display)

        self-vote?    (fn [{:keys                 [action params]
                            {:keys [id rank act]} :params
                            :as                   button}]
                        (and (#{:rank-player} action)
                             (= user-id id)))
        valid-button? (fn [{:keys                 [action params disabled?]
                            {:keys [id rank act]} :params
                            :as                   button}]
                        (cond
                          (#{:rank-player} action)
                          (and
                           (not= user-id id)
                           (nil? (get-in player-ranks [user-id act rank]))
                           (not= id (get-in player-ranks [user-id act :best])))
                          :else
                          (not disabled?)))
        ]
    (println extra-details)
    [:div.game-table
     [:div.current {}
      [:div.active-area {}
       [:div.stage-info {}
        [:div.stage-name (str stage-name)]
        [:div.stage-focus (str stage-focus)]]
       [:div.x-card {:class (if x-carded? "active" "inactive")}
        [:a {:on-click #(dispatch [:<-game/action! :x-card])} "X"]]
       [:div.card {:class (str " "
                               (if x-carded?
                                 "x-carded"))}
          (-> (get-in display [:card :text])
              (m/md->hiccup)
              (m/component))
        (map (fn [{:keys [name value label generator]}]
               (with-meta
                 [:div.user-input
                  [:label [:strong label]]
                  [:br]
                  [:p [:em value]]
                  ;; [:input {:name name :value value}]
                  ]
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
                                          (dispatch [:<-game/action! action params])) }
                [:a {} text]]
               {:key (str action params)}))
           (get-in display [:actions]))]]
      ]
     [:div.extras
      [:div.voting-area
       (if voting-active?
         (map (fn [{:keys [id user-name dead? agent-name agent-codename agent-role]}]
                (let [total-score (apply + (vals (player-scores id)))]
                  (with-meta
                    [:div.player
                     [:div.player-name
                      {:title agent-name}
                      (str "[ " total-score " ] " (if agent-name (str agent-codename ", " agent-role " ")) " (" user-name ")")]
                     [:div.score-actions
                      ;; TODO - maybe this logic should come from gamemaster
                      (if-not (= id user-id)
                        [:a.downvote-player.button {:on-click #(dispatch [:<-game/action! :downvote-player {:player-id id}])} " - "])
                      [:div.score (str (get-in player-scores [id user-id]))]
                      (if-not (= id user-id)
                        [:a.upvote-player.button {:on-click #(dispatch [:<-game/action! :upvote-player {:player-id id}])} " + "])
                      ]]
                    {:key id})))
              all-players))]
      [:div.company
       [:h2 "Round Themes"]
       [:ul
        (map
         (fn [val] (with-meta [:li val] {:key val}))
         (:values company))]]
      (if voting-active?
        [:div.mission-details
         [:h2 "More Details"]
         [:p (str (:text mission))]])
      (if (and voting-active? extra-details)
        [:div.extra-details
         (map (fn [{:keys [title items]}]
                 (with-meta
                   [:div.detail
                    [:h2 title]
                    [:ul
                     (map #(with-meta [:li %] {:key %}) items)]]
                   {:key title}))
               extra-details
               )])
      (map (fn [{conf  :confirm
                 :keys [action class text]}]
             (with-meta (vector :div.extra-action {:class class} [:a.button {:on-click #(if (or (not conf) (js/confirm "Are you sure?"))
                                                                                          (dispatch [:<-game/action! action]))} text]) {:key action}))
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
        [:a {:on-click #(dispatch [:<-game/action! :x-card])} "X"]]
       [:div.card {:class (str (name (get-in display [:card :state]))
                               " "
                               (if x-carded?
                                 "x-carded"))}
          (-> (get-in display [:card :text])
              (m/md->hiccup)
              (m/component))]
         [:div.actions
          (map (fn [{:keys [action text]}] (with-meta (vector :div.action [:a {:on-click #(dispatch [:<-game/action! action])} text]) {:key action}))
               (get-in display [:actions]))]]
      ]
     [:div.extras
      [:img {:src (str (:text queen))}]
      (map (fn [{conf :confirm
                 :keys [action class text]}]
             (with-meta (vector :div.extra-action {:class class} [:a.button {:on-click #(if (or (not conf) (js/confirm "Are you sure?"))
                                                                                          (dispatch [:<-game/action! action]))} text]) {:key action}))
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

(defn layout [body]
  [:div.page {}
   [:header
    {}
    #_[:h1 "Tenkiwi"]]
   [:article {} body]
   [:footer {}
    [:p "This work is based on "
     [:a {:href "http://www.forthequeengame.com/"}
      "For the Queen"]
     ", product of Alex Roberts and Evil Hat Productions, and licensed for our use under the "
     [:a {:href "http://creativecommons.org/licenses/by/3.0/"}
      "Creative Commons Attribution 3.0 Unported license"]
     ]
    ]])

(defn -connecting-panel []
  (let []
    [:div "Connecting to server..."]))

(defn main-panel []
  (let [user (re-frame/subscribe [:user])
        room (re-frame/subscribe [:room])
        game (get-in @user [:current-room :game :game-type])]
    (layout
     (cond
       game [game-panel]
       (get @user :current-room) [lobby-panel]
       (get @user :connected?) [join-panel]
       :else [-connecting-panel]))))
