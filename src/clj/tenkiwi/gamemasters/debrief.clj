(ns tenkiwi.gamemasters.debrief
  "This is the game logic for debrief game"
  (:require
   [clojure.string :as string]
   [tenkiwi.tables.debrief :as tables]
   [tenkiwi.util :as util :refer [inspect push-uniq]]
   [tenkiwi.rules.player-order :as player-order]
   [tenkiwi.rules.prompt-deck :as prompt-deck]
   [tenkiwi.rules.x-card :as x-card]
   [tenkiwi.rules.word-bank :as word-bank]
   [tenkiwi.rules.voteboard :as voteboard]
   [tenkiwi.rules.character-sheets :as character-sheets]
   ))

(def valid-active-actions #{:rank-player :regen :pass :discard :undo :done :x-card :end-game :upvote-player :downvote-player :leave-game})
(def valid-inactive-actions #{:rank-player :x-card :undo :leave-game :upvote-player :downvote-player})

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

(defn score-ranks
  "Finds scores for player id based on best/worst ranking from each other player
  / round."
  ([player-id ranks]
   (score-ranks player-id ranks [0 1 2]))
  ([player-id ranks stages]
   (apply +
          (for [stage (map str stages)
                rater (remove #{player-id} (keys ranks))]
            (cond
              (#{player-id} (get-in ranks [rater stage :best]))
              10
              (#{player-id} (get-in ranks [rater stage :worst]))
              3
              :else
              6)))))

(defn score-players [{:keys [player-ranks] :as game}]
  (let [base-scores (voteboard/->total-scores game)
        final-scores (reduce #(update %1 %2 + (score-ranks %2 player-ranks))
                             base-scores
                             (keys base-scores))]
    final-scores))

(defn extract-vars [{:keys [company
                            dossiers mission]
                     :as   game}]
  (let [next-player   (:id (player-order/next-player game))
        prev-player   (:id (player-order/previous-player game))
        player-names  (character-sheets/->player-names game)
        secondaries   (:secondary-objectives mission [])
        complications (:complications mission [])
        scores        (score-players game)
        values        (:values company)]
    {:leader-name    (get-in player-names [:leader] "")
     :player-left    (get-in player-names [prev-player] "")
     :player-right   (get-in player-names [next-player] "")
     :scoreboard     (string/join "\n"
                                (map #(str
                                       "* "
                                       (player-names %)
                                       " - "
                                       (scores %))
                                     (keys scores)))
     :value-0        (nth values 0 "unknown")
     :value-1        (nth values 1 "unknown")
     :value-2        (nth values 2 "unknown")
     :primary        (:primary-objective mission)
     :secondary      (first (shuffle secondaries))
     :secondary-0    (nth secondaries 0 "unknown")
     :secondary-1    (nth secondaries 1 "unknown")
     :secondary-2    (nth secondaries 2 "unknown")
     :complication   (first (shuffle complications))
     :complication-0 (nth complications 0 "unknown")
     :complication-1 (nth complications 1 "unknown")
     :complication-2 (nth complications 2 "unknown")
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

;; TODO: Use pluck
(defn dossier-card
  ([dossier-template generators player]
   (dossier-card dossier-template generators player {}))
  ([dossier-template generators player defaults]
   (let [generator-list (->> (clojure.string/split (:inputs dossier-template) #"\s\s")
                             (map #(clojure.string/split % #":"))
                             (into {}))
         pluck-value    (fn [keyname]
                          (or (get defaults keyname)
                              (-> generators
                                  (get keyname [{:text "unknown"}])
                                  shuffle
                                  first
                                  :text)))]
     (merge dossier-template
            {:id     :player-dossier
             :inputs (mapv #(hash-map :name (first %)
                                      :label (last %)
                                      :value (pluck-value (first %)))
                           generator-list)}))))

;; TODO: XSS danger?
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

(defn player-button [game params {:keys [id user-name]}]
  (let [player-names (character-sheets/->player-names game)]
    {:action  :rank-player
     :params  (merge params {:id id})
     :confirm true
     :text    (player-names id)}))

(defn upvote-current-player-button [game {:keys [id user-name]}]
  (let [player-names (character-sheets/->player-names game)]
    {:action :upvote-player
     :params {:player-id id}
     :text   (str "Upvote " (player-names id))}))

(defn downvote-current-player-button [game {:keys [id user-name]}]
  (let [player-names (character-sheets/->player-names game)]
    {:action :downvote-player
     :params {:player-id id}
     :text   (str "Downvote " (player-names id))}))

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
        downvoting     (one-per-act downvoting)
        round-start-qs (get questions (str "^" round) [])
        round-end-qs   (get questions (str round "$") [])
        remainder      (- card-count (+ (count round-start-qs)
                                        (count round-end-qs)))]
    (into []
          (concat
           [(get act-starts round)]
           round-start-qs
           (take remainder (shuffle (questions round)))
           round-end-qs
           [(get upvoting round) (get downvoting round)]))))

(defn build-draw-deck [{intro-cards :intro
                        questions   :question
                        missions    :mission
                        :as         decks}
                       {:keys [mission-details
                               players
                               card-count]}]
  (let [generators       (->> decks :generator (group-by :act))
        act-names        (-> decks :act-name (one-per-act :text))
        dossier-template (->> decks :dossier first)]
    (into []
          (concat (rest intro-cards)
                  (map (partial character-sheets/placeholder dossier-template) players)
                  (:briefing-cards mission-details)
                  (mapcat #(build-round % card-count decks)
                          (keys act-names))
                  [(:ending-card mission-details)]))))

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
                                :upvoting   (mapv (partial player-button game {:rank :best
                                                                               :act  act}) all-players)
                                :downvoting (mapv (partial player-button game {:rank :worst
                                                                               :act  act}) all-players)
                                :dossier    [done-action]
                                (if can-finish?
                                  [done-action pass]
                                  [pass]))]
    (assoc game
           :active-display
           (merge display
                  {:card              (replace-vars game card)
                   :extra-actions     [undo-action leave-game-action]
                   :available-actions valid-active-actions
                   :actions           (if x-card-active?
                                        (push-uniq next-actions discard-action)
                                        next-actions)}))))

(defn render-inactive-display [{:keys                   [dossiers active-display]
                                {:keys [active-player
                                        card]}          :display
                                :as                     game}]
  (let [card-type        (:type card)
        disabled-actions [{:text      (:text (waiting-for active-player))
                           :disabled? true}]
        extra-actions    [(upvote-current-player-button dossiers active-player)
                          (downvote-current-player-button dossiers active-player)]]
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

;; TODO: Remove when fully moved to new display system
(defn render-backwards-compatibility [{:as state
                                       {:keys [sheets
                                               player-scores
                                               active-player]} :display}]
  (assoc state
         :dossiers      sheets
         :player-scores player-scores
         :active-player active-player))

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
      voteboard/render-display
      character-sheets/render-display
      render-backwards-compatibility
      render-active-display
      render-inactive-display))

(defn start-game [room-id {:keys [game-url]
                           :or   {}}
                  {:keys [players] :as room}]
  (let [decks            (util/gather-decks game-url)
        generators       (->> decks :generator (group-by :act))
        mission-details  (prepare-mission decks)
        npcs             [{:user-name "NPC"
                           :id        :leader
                           :npc?      true}]
        all-players      (concat (into [] players)
                                 npcs)
        dossier-template (->> decks :dossier first)

        initial-state (-> {:game-type :debrief}
                          (player-order/initial-state room)
                          (x-card/initial-state {})
                          (character-sheets/initial-state {:name-key   :agent-codename
                                                           :intro-card dossier-template})
                          (voteboard/initial-state {:players all-players})
                          (word-bank/initial-state {:word-banks    (:story-details mission-details)
                                                    :word-bank-key :extra-details
                                                    :generators    generators})
                          (character-sheets/initial-state {:name-key   :agent-codename
                                                           :intro-card dossier-template
                                                           :players    players})
                          (prompt-deck/initial-state {:features {:everyone true}
                                                      :deck     (build-draw-deck decks
                                                                                 {:mission-details mission-details
                                                                                  :card-count      11
                                                                                  :players         players})}))
        initial-state (character-sheets/set-sheet! initial-state
                                                   {:id :leader}
                                                   {:locked?        true
                                                    :user-name      "NPC"
                                                    :agent-name     (tables/random-name)
                                                    :agent-codename (word-bank/->pluck initial-state "leader-codename")
                                                    :agent-role     (word-bank/->pluck initial-state "leader-role")})

        act-names     (-> decks :act-name (one-per-act :text))
        focus-names   (-> decks :focus-name (one-per-act :text))

        company  {:name   (word-bank/->pluck initial-state "company")
                  :values (word-bank/->pluck initial-state "value" 3)}

        new-game (merge
                  initial-state
                  {:player-ranks     (zipmap
                                      (map :id players)
                                      (cycle [(zipmap (keys act-names)
                                                      (cycle [{:best nil :worst nil}]))]))
                   :all-players      all-players
                   :game-type        :debrief
                   :stage-names      {:intro            "Introduction"
                                      :mission-briefing "Mission Briefing"
                                      :dossier          "Character Intros"
                                      :question         "{act-name}"
                                      :act-start        "{act-name}"
                                      :downvoting       "{act-name} (Voting)"
                                      :upvoting         "{act-name} (Voting)"}
                   :act-names        act-names
                   :focus-names      focus-names
                   :stage            :intro
                   :stage-name       "Introduction"
                   :stage-focus      ""
                   ;; :dossiers         dossiers
                   :mission          mission-details
                   :company          company
                   :dossier-template dossier-template
                   :-generators      generators})]
    (render-game-display new-game)))

(defn extract-dossier [{:keys [inputs]}]
  (zipmap (map keyword (map :name inputs))
          (map :value inputs)))

(defn render-testing [state key]
  (println key "=> "(get-in state key))
  state)

(defn finish-card [game]
  (let [active-player    (player-order/active-player game)
        previous-card    (prompt-deck/active-card game)
        next-state       (-> game
                             ;; Order matters - lock before transition player
                             character-sheets/maybe-lock-sheet!
                             player-order/activate-next-player!
                             word-bank/regen-word-banks!
                             x-card/reset-x-card!
                             prompt-deck/draw-next-card!)]
    (-> next-state
        (assoc :-last-state game)
        render-game-display)))

(defn discard-card [game]
  (let [next-game       (-> game
                            ;; Order matters - lock before next card
                            character-sheets/maybe-lock-sheet!
                            prompt-deck/draw-next-card!
                            word-bank/regen-word-banks!
                            x-card/reset-x-card!
                            (assoc :-last-state game))
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
      (assoc :-last-state game)
      render-game-display))

;; TODO: How much stress does this add to duratom?
(defn undo-card [game]
  (let [{:keys [-last-state]} game
        new-state (assoc-in -last-state
                            [voteboard/$ :scores]
                            (voteboard/->scores game))]
    (render-game-display new-state)))

(defn rank-player
  [voter-id
   {:keys [id rank act]}
   {:keys [player-ranks] :as game}]
  (let [votes-remaining? (fn [x]
                           (let [ranks (get x :player-ranks)]
                            (some nil? (map #(get-in ranks [% act rank])
                                            (keys ranks)))))
        maybe-finish     #(if (votes-remaining? %)
                            %
                            (finish-card %))]
    (if (and (not= id voter-id)
             (nil? (get-in player-ranks [voter-id act rank])))
     (-> game
         (assoc-in [:player-ranks voter-id act rank] id)
         maybe-finish)
     game)))

(defn upvote-player
  [voter-id
   {:keys [player-id]}
   game]
  (let [current-score (voteboard/current-score game [player-id voter-id])
        player-name   (-> (character-sheets/->player-names game)
                         (get player-id))]
    (if current-score
      (-> (voteboard/upvote! game [player-id voter-id])
          (assoc-in [:broadcasts] [[:->toast/show! (str "👍  " player-name " got upvoted.")]])
          render-game-display)
      game)))

(defn downvote-player
  [voter-id
   {:keys [player-id]}
   game]
  (let [current-score (voteboard/current-score game [player-id voter-id])
        player-name   (-> (character-sheets/->player-names game)
                         (get player-id))]
    (if current-score
      (-> (voteboard/downvote! game [player-id voter-id])
          (assoc-in [:broadcasts] [[:->toast/show! (str "👎  " player-name " got downvoted.")]])
          render-game-display)
      game)))

(defn x-card [game]
  (-> (x-card/activate-x-card! game)
      render-game-display))

(defn end-game [game]
  nil)

(defn tick-clock [game]
  ;; Nothing
  game)

;;; FIXME: Reaches into prompt deck state. :(
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
                        :done            finish-card
                        :x-card          x-card
                        :discard         discard-card
                        :pass            pass-card
                        :undo            undo-card
                        :regen           (partial regen-card params)
                        ;; TODO - work out upvote/downvote UI for players
                        :upvote-player   (partial upvote-player uid params)
                        :downvote-player (partial downvote-player uid params)
                        :rank-player     (partial rank-player uid params)
                        :tick-clock      tick-clock
                        ;; TODO allow players to leave game without ending
                         ;;; change action text
                        :leave-game      end-game
                        :end-game        end-game)
        execute       (if-active-> uid action do-next-state)]
    (try
      (execute game)
      (catch Exception e (println e)))))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
