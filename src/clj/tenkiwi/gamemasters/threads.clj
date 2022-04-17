(ns tenkiwi.gamemasters.threads
  "This is the game logic for therads game"
  (:require
   [clojure.string :as string]
   [tenkiwi.tables.debrief :as tables]
   [tenkiwi.util :as util :refer [inspect push-uniq]]
   [tenkiwi.rules.player-order :as player-order]
   [tenkiwi.rules.prompt-deck :as prompt-deck]
   [tenkiwi.rules.x-card :as x-card]
   [tenkiwi.rules.word-bank :as word-bank]
   [tenkiwi.rules.stress :as stress]
   [tenkiwi.rules.character-sheets :as character-sheets]
   [tenkiwi.rules.undoable :as undoable]
   ))

(def valid-active-actions #{:regen :pass :discard :undo :done :x-card :end-game :upstress-player :downstress-player :leave-game})
(def valid-inactive-actions #{:x-card :undo :leave-game :upstress-player :downstress-player})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))

(def done-action
  {:action :done
   :text   "Finish Turn"})

(def regen-action
  {:action :regen
   :params {}
   :text   "Shuffle"})

(def leave-game-action
  {:action  :leave-game
   :confirm true
   :text    "End Game Now"})

(def discard-action
  {:action :discard
   :text   "[X] Discard this..."})

(def end-game-action
  {:action  :end-game
   :text    "End the Game"})

(def undo-action
  {:action  :undo
   :text    "Undo Last"})

(defn extract-vars [{:keys []
                     :as   game}]
  (let [next-player   (:id (player-order/next-player game))
        prev-player   (:id (player-order/previous-player game))
        player-names  (character-sheets/->player-names game)]
    {:player-left    (get-in player-names [prev-player] "")
     :player-right   (get-in player-names [next-player] "")
     }))

(defn replace-vars [game str-or-card]
  (let [text      (if (string? str-or-card)
                    str-or-card
                    (:text str-or-card))
        game-vars (extract-vars game)
        replaced  (string/replace (or text "")
                                          #"\{([^\}]+)\}"
                                          #(get game-vars (keyword (nth % 1))
                                                (nth % 1)))]
    (cond
      (string? str-or-card)
      replaced
      (map? str-or-card)
      (assoc str-or-card :text replaced)
      :else str-or-card)))

(defn waiting-for
  ([{:keys [user-name]}]
   {:id    "waiting"
    :type :inactive
    :text  (str "It is " user-name "'s turn...")})
  ([{:keys [user-name]}
    extra-text]
   {:id    "waiting"
    :type :inactive
    :text  (str extra-text "\n\n" "It is " user-name "'s turn...")}))

(defn build-mission-details [mission-briefing-cards
                             {:keys [text secondary-objectives complications story-details] :as card}]
  (let [briefing       (->> (string/split text #"\n\n")
                      (map #(hash-map :text % :type :mission-briefing)))
        mission-open   (get mission-briefing-cards "0")
        mission-wrapup (get mission-briefing-cards "2")
        mission-ending (-> mission-briefing-cards
                           (get "end" [{:text "{scoreboard}"}])
                           first)]
    (-> card
        (assoc :briefing-cards (concat mission-open briefing mission-wrapup))
        (assoc :ending-card    (assoc mission-ending :type :end))
        (update :secondary-objectives #(->> (string/split % #"\s\s") (map string/trim)))
        (update :complications #(->> (string/split % #"\s\s") (map string/trim)))
        (update :complications shuffle)
        (update :secondary-objectives shuffle))))

(defn one-per-act
  ([act-collection]
   (let [grouped (group-by :act act-collection)]
     (zipmap (keys grouped)
             (map first (vals grouped)))))
  ([act-collection func]
   (let [grouped (group-by :act act-collection)]
     (zipmap (keys grouped)
             (map #(-> % first func) (vals grouped))))))

(defn build-round [round card-count {:keys [question act-start upvoting downvoting]
                                     :as   decks}]
  (let [round          (if (string? round) round (str round))
        questions      (group-by :act question)
        act-starts     (one-per-act act-start)
        upvoting       (one-per-act upvoting)
        downvoting     (one-per-act downvoting)]
    (into []
          (concat
           [(get act-starts round)]
           (take card-count (shuffle (questions round)))
           [(get upvoting round) (get downvoting round)]))))

(defn build-draw-deck [{intro-cards :intro
                        :as         decks}
                       {:keys [players]}]
  (let []
    (into []
          (concat intro-cards
                  #_(mapcat #(build-round % card-count decks)
                          (keys act-names))
                  #_[(:ending-card mission-details)]))))

(defn prepare-mission [{missions :mission
                        :as decks}]
  (let [mission-briefing (->> decks :mission-briefing (group-by :act))
        mission (first (shuffle missions))]
    (build-mission-details mission-briefing mission)))

(defn render-stage-info [{:keys [company
                                 act-names
                                 stage-names
                                 focus-names]
                          :as   game}]
  (let [next-card       (prompt-deck/active-card game)
        next-stage      (get next-card :type :intro)
        next-act        (clojure.string/replace (str (get next-card :act "0"))
                                                #"[^\d]"
                                                "")
        next-stage-name (-> (get stage-names next-stage "Introduction")
                            (clojure.string/replace #"\{act-name\}" (get act-names next-act)))

        next-stage-focus (cond (#{:question :act-start} next-stage)
                               (replace-vars game (get focus-names next-act (str "{value-" next-act "}")))
                               :else "")]
    (assoc game
           :stage       next-stage
           :stage-name  next-stage-name
           :stage-focus next-stage-focus)))

(defn render-active-display [{:as                      game
                              {:as   display
                               :keys [card
                                      active-player
                                      next-player
                                      x-card-active?]} :display}]
  (let [{:keys [all-players]} game
        act                   (:act card)
        next-stage            (or (:type card) :intro)
        ;; Don't allow done-action for #everyone cards until they are passed around
        can-finish?           (prompt-deck/->everyone? game)
        pass                  {:action :pass
                               :text   (str "Pass card to " (:user-name next-player))}
        next-actions          (case next-stage
                                :end        [pass end-game-action]
                                :dossier    [done-action]
                                [done-action pass])]
    (assoc game
           :active-display
           {:card              (replace-vars game card)
            :extra-actions     [undo-action leave-game-action]
            :available-actions valid-active-actions
            :actions           (if x-card-active?
                                 (push-uniq next-actions discard-action)
                                 next-actions)})))

(defn render-inactive-display [{:keys                   [dossiers active-display]
                                {:keys [active-player
                                        card]}          :display
                                :as                     game}]
  (let [card-type        (:type card)
        disabled-actions [{:text      (:text (waiting-for active-player))
                           :disabled? true}]
        extra-actions    []]
    (assoc game
           :inactive-display
           (cond
             (#{:act-start :question :intro :mission-briefing} card-type)
             (-> active-display
                 (assoc :available-actions valid-inactive-actions)
                 (assoc :actions disabled-actions)
                 (update :actions #(into % extra-actions)))
             (#{:dossier} card-type)
             {:card              (waiting-for active-player "Introductions are being made.")
              :available-actions valid-inactive-actions
              :extra-actions     [undo-action leave-game-action]}
             :else
             (-> active-display
                 (assoc :available-actions valid-inactive-actions))))))

(defn render-test [game]
  (println (keys game))
  (println (keys (:display game)))
  game)

(defn render-game-display [game]
  (-> (render-stage-info game)
      player-order/render-display
      prompt-deck/render-display
      x-card/render-display
      word-bank/render-display
      character-sheets/render-display
      stress/render-scoreboard-display
      render-active-display
      render-inactive-display))

(defn select-game [room-id {:keys [game-url shifts episode]
                            :as   params
                            :or   {}}
                   {:keys [players] :as room}]
  (let [{:keys [opening]} (util/gather-decks game-url)
        sheet-template    {:nickname "Nickname"}
        new-game          {:game-type      :threads
                           :configuration  {:params {:shifts shifts}
                                            :inputs [{:type    :select
                                                      :label   "Episode Shifts"
                                                      :name    :shifts
                                                      :options (mapv #(hash-map :value % :name %)
                                                                     (range -8 9))}
                                                     {:type    :select
                                                      :label   "Pick an Episode"
                                                      :name    :episode
                                                      :options (concat [{:value :random :name "Random"}]
                                                                       (mapv #(hash-map :value (:id %) :name (:text %))
                                                                             opening))}]}
                           :sheet-template sheet-template}]
    new-game))

(defn start-game [room-id {:keys [game-url shifts episode]
                           :or   {}}
                  {:keys [players] :as room}]
  (let [decks          (util/gather-decks game-url)
        generators     (->> decks :generator (group-by :concept))
        ;; mission-details  (prepare-mission decks)
        ;; TODO: Remove?
        sheet-template {:text "Stuff happens." :inputs "Foo: bar"}

        initial-state (-> {:game-type :threads}
                          (player-order/initial-state room)
                          (x-card/initial-state {})
                          (character-sheets/initial-state {:name-key   :nickname
                                                           :intro-card sheet-template
                                                           :players    players})
                          (stress/initial-state {:players players})
                          (undoable/initial-state {:skip-keys [:display :active-display :inactive-display]})
                          (word-bank/initial-state {:word-banks    "ingredient: Ingredients"#_(:story-details mission-details)
                                                    :word-bank-key :extra-details
                                                    :generators    generators})
                          (prompt-deck/initial-state {:features {}
                                                      :deck     (build-draw-deck decks
                                                                                 {:players         players})}))


        new-game (merge
                  initial-state
                  {:game-type        :threads
                   :players          players
                   :stage-names      {:intro            "Introduction"
                                      :mission-briefing "Mission Briefing"
                                      :dossier          "Character Intros"
                                      :question         "{act-name}"
                                      :act-start        "{act-name}"
                                      :downvoting       "{act-name} (Voting)"
                                      :upvoting         "{act-name} (Voting)"}
                   :stage            :intro
                   :stage-name       "Introduction"
                   :stage-focus      ""})]
    (render-game-display new-game)))

(defn extract-dossier [{:keys [inputs]}]
  (zipmap (map keyword (map :name inputs))
          (map :value inputs)))

(defn finish-card [game]
  (let [active-player    (player-order/active-player game)
        previous-card    (prompt-deck/active-card game)
        next-state       (-> game
                             ;; Order matters - lock before transition player
                             character-sheets/maybe-lock-sheet!
                             player-order/activate-next-player!
                             word-bank/regen-word-banks!
                             x-card/reset-x-card!
                             prompt-deck/draw-next-card!
                             (undoable/checkpoint! game))]
    (render-game-display next-state)))

(defn discard-card [game]
  (let [next-game       (-> game
                            ;; Order matters - lock before next card
                            character-sheets/maybe-lock-sheet!
                            prompt-deck/draw-next-card!
                            word-bank/regen-word-banks!
                            x-card/reset-x-card!
                            (undoable/checkpoint! game))
        next-card       (prompt-deck/active-card next-game)]
    ;; Don't allow discard if deck empty
    (if next-card
      (render-game-display next-game)
      game)))

(defn pass-card [game]
  (-> game
      prompt-deck/card-passed!
      word-bank/regen-word-banks!
      player-order/activate-next-player!
      (undoable/checkpoint! game)
      render-game-display))

;; TODO: How much stress does this add to duratom?
(defn undo-card [game]
  (let [new-state (undoable/undo! game)]
    (render-game-display new-state)))

(defn upstress-player
  [voter-id
   {:keys [player-id]}
   game]
  (let [current-score (stress/current-score game [player-id])
        player-name   (-> (character-sheets/->player-names game)
                         (get player-id))]
    (if current-score
      (-> (stress/upstress! game [player-id])
          (assoc-in [:broadcasts] [[:->toast/show! (str "😬 " player-name " took stress.")]])
          render-game-display)
      game)))

(defn downstress-player
  [voter-id
   {:keys [player-id]}
   game]
  (let [current-score (stress/current-score game [player-id])
        player-name   (-> (character-sheets/->player-names game)
                         (get player-id))]
    (if current-score
      (-> (stress/downstress! game [player-id])
          (assoc-in [:broadcasts] [[:->toast/show! (str "😅 " player-name " removed stress.")]])
          render-game-display)
      game)))

(defn x-card [game]
  (-> (x-card/activate-x-card! game)
      (undoable/checkpoint! game)
      render-game-display))

(defn end-game [game]
  nil)

(defn tick-clock [game]
  ;; Nothing
  game)

(defn regen-card [params
                  {:keys [stage]
                   :as   game}]
  (let [active-player  (player-order/active-player game)]
    (cond
      (#{:dossier} stage)
      (-> game
          (character-sheets/regen! active-player params)
          render-game-display)
      :else
      game)))

(defn if-active-> [uid action do-next-state]
  (fn [{:as game}]
    (let [active-player? (player-order/is-active? game {:id uid})]
      (if (valid-action? active-player? action)
        (do-next-state game)
        game))))

(defn take-action [{:keys [uid room-id action params]} {:keys [game]}]
  (let [do-next-state (case action
                        :done              finish-card
                        :x-card            x-card
                        :discard           discard-card
                        :pass              pass-card
                        :undo              undo-card
                        :regen             (partial regen-card params)
                        ;; TODO - work out upvote/downvote UI for players
                        :upstress-player   (partial upstress-player uid params)
                        :downstress-player (partial downstress-player uid params)
                        :tick-clock        tick-clock
                        ;; TODO allow players to leave game without ending
                         ;;; change action text
                        :leave-game        end-game
                        :end-game          end-game)
        execute       (if-active-> uid action do-next-state)]
    (try
      (execute game)
      (catch Exception e (println e)))))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
