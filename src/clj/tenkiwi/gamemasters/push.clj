(ns tenkiwi.gamemasters.push
  "This is the game logic for Push SRD games"
  (:require
   [clojure.string :as string]
   [tenkiwi.util :as util :refer [inspect push-uniq]]
   [tenkiwi.rules.player-order :as player-order]
   [tenkiwi.rules.prompt-deck :as prompt-deck]
   [tenkiwi.rules.x-card :as x-card]
   [tenkiwi.rules.word-bank :as word-bank]
   [tenkiwi.rules.game-stages :as game-stages]
   [tenkiwi.rules.oracle-box :as oracle-box]
   [tenkiwi.rules.character-sheets :as character-sheets]
   [tenkiwi.rules.undoable :as undoable]
   ))

(def valid-active-actions #{:next-phase :regen :consult :pass :discard :undo :done :x-card :change-stage :end-game :leave-game})
(def valid-inactive-actions #{:next-phase :regen :x-card :consult :undo :change-stage :leave-game})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))

(def game-definition
  {:features
   {:character-sheets :configurable
    :turns            :skippable-phases
    :intro-stage      true
    :word-bank        true
    :x-card           true}
   :screens
   [{:stage   :intro
     :prompt  :card-with-actions
     :views   [[:intro] [:configure-character] [:extra-actions]]}
    {:stage   :game
     :prompt  :card
     :views   [[:actions :wordbank] [:oracle :characters] [:extra-actions]]}]})

(def turn-definition
  {:intro {:everyone? true}
   :game
   {:phases [:encounter :descriptions :actions]}})

(def done-action
  {:action :done
   :text   "Finish Turn"})

(defn next-stage-action [next-stage]
  {:action :change-stage
   :params {:stage next-stage}
   :text   "Continue"})

(def next-phase-action
  {:action :next-phase
   :text   "Next Phase"})

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

(defn extract-vars [{:keys [matrix challenge]
                     :as   game}]
  (let [next-player  (:id (player-order/next-player game))
        prev-player  (:id (player-order/previous-player game))
        player-names (character-sheets/->player-names game)]
    {:previous-player (get-in player-names [prev-player] "")
     :matrix          (or matrix "")
     :challenge       (or challenge "")
     :next-player     (get-in player-names [next-player] "")}))

(defn replace-vars [game str-or-card]
  (let [text      (if (string? str-or-card)
                    str-or-card
                    (:text str-or-card))
        game-vars (extract-vars game)
        replaced  (string/replace (or text "")
                                  #"\{([^\}]+)\}"
                                  #(get game-vars (keyword (nth % 1))
                                        (or
                                         (word-bank/->pluck game (nth % 1))
                                         (str "-" (nth % 1)))))]
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

(defn one-per-key
  ([key collection]
   (let [grouped (group-by key collection)]
     (zipmap (keys grouped)
             (map first (vals grouped)))))
  ([key collection func]
   (let [grouped (group-by key collection)]
     (zipmap (keys grouped)
             (map #(-> % first func) (vals grouped))))))

(defn build-draw-deck [{intro-cards :intro
                        missions    :mission
                        :as         decks}
                       {:keys [mission-details
                               scene-count]}]
  (let [opening (->> decks :scene-opening first)
        description (->> decks :scene-description first)
        actions (->> decks :scene-actions first)]
    (into []
          (concat (mapcat
                   (fn [matrix]
                     [(assoc opening :matrix matrix)
                      (assoc description :matrix matrix)
                      (assoc actions :matrix matrix)])
                   (take scene-count (:matrix mission-details)))
                  ))))

(defn render-stage-info [{:keys []
                          :as   game}]
  (let [next-card       (prompt-deck/active-card game)]
    (-> game
        (assoc-in [:episode] (get-in game [:mission-details])))))

(defn render-active-display [{:as                      game
                              {:as   display
                               :keys [card
                                      active-player
                                      next-player
                                      x-card-active?]} :display}]
  (let [{:keys []}   game
        act          (:act card)
        next-stage   (game-stages/->stage game)
        ;; Don't allow done-action for #everyone cards until they are passed around
        can-finish?  (prompt-deck/->everyone? game)
        pass         {:action :pass
                      :text   (str "Pass card to " (:user-name next-player))}
        next-actions (case next-stage
                       :intro     [(next-stage-action :character)]
                       :character [(next-stage-action :mission)]
                       :mission   [(next-stage-action :game)]
                       (if can-finish?
                         [next-phase-action pass]
                         [next-phase-action pass]))
        updated-card (replace-vars game card)]
    (-> game
        ;; Revisit which way is appropriate for this game
        ;; Currently we use the single display
        (assoc-in [:display :card] updated-card)
        ;;
        (assoc
            :active-display
            (merge display
                   {:card              updated-card
                    :extra-actions     [undo-action leave-game-action]
                    :available-actions valid-active-actions
                    :actions           (if x-card-active?
                                         (push-uniq next-actions discard-action)
                                         next-actions)})))))

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
           (-> active-display
               (assoc :available-actions valid-inactive-actions)))))

(def $decks :-decks)

(defn render-phase-prompt [{:keys  [$decks]
                            decks $decks
                            :as    game}]
  (let [opening (->> decks :scene-opening first)
        description (->> decks :scene-description first)
        actions (->> decks :scene-actions first)
        prompt  (case (game-stages/->phase game)
                  :encounter opening
                  :descriptions description
                  :actions actions
                  {:text ""})]
    (-> game
        (assoc-in [:display :card] prompt)
        (assoc-in [:display :challenge] (:challenge game))
        (assoc-in [:display :matrix] (:matrix game)))))

(defn render-test [game]
  (println (keys game))
  (println (keys (:display game)))
  game)

(defn render-game-display [game]
  (-> (render-stage-info game)
      player-order/render-display
      ;; prompt-deck/render-display
      render-phase-prompt
      x-card/render-display
      word-bank/render-display
      oracle-box/render-oracle-display
      game-stages/render-display
      ;; render-test
      character-sheets/render-display
      render-active-display
      render-inactive-display))

(defn prepare-mission [{:keys [matrix mission agenda intro]
                        :as   decks}]
  (let [matrix (mapv :text (shuffle matrix))
        mission (rand-nth mission)
        agenda (rand-nth agenda)]
    {:mission mission
     :mission-text (:text mission)
     :intro  intro
     :agenda agenda
     :agenda-text (:text agenda)
     :matrix  matrix}))

(defn start-game [room-id {:keys [game-url]
                           :or   {}}
                  {:keys [players] :as room}]
  (let [decks                  (util/gather-decks game-url)
        generators             (->> decks :generator (group-by :concept))
        mission-details        (prepare-mission decks)
        {action-roll "action"
         oracle-roll "oracle"
         :as         oracles}  (->> decks :oracle (group-by :concept))
        {action-desc "action"
         oracle-desc "oracle"} (-> (group-by :concept (:oracle-description decks))
                                   (util/update-values first)
                                   (util/update-values :text))
        dossier-template       (->> decks :dossier first)



        initial-state (-> {:game-type       :push
                           :mission-details mission-details}
                          (player-order/initial-state {:players players})
                          (x-card/initial-state {})
                          (oracle-box/initial-state {:id          :oracle
                                                     :push?       true
                                                     :title       "Oracle"
                                                     :description oracle-desc
                                                     :table       (group-by :number oracle-roll)})
                          (oracle-box/initial-state {:id          :action
                                                     :push?       true
                                                     :title       "Action"
                                                     :description action-desc
                                                     :table       (group-by :number action-roll)})
                          (game-stages/initial-state {:initial-stage :intro
                                                      :phases        {:encounter    {}
                                                                      :descriptions {}
                                                                      :actions      {}}
                                                      :stages        {:intro
                                                                      {:title  "Introduction"
                                                                       :screen :intro}
                                                                      :character
                                                                      {:title  "Character"
                                                                       :screen :character}
                                                                      :mission
                                                                      {:title  "Mission"
                                                                       :screen :mission}
                                                                      :game
                                                                      {:title  "Game"
                                                                       :screen :game
                                                                       :phases [:encounter :descriptions :actions]}}})
                          (undoable/initial-state {:skip-keys [:display :active-display :inactive-display]})
                          (word-bank/initial-state {:word-banks      [{:title "Challenge"
                                                                       :name  "challenge"
                                                                       :group :challenge}
                                                                      {:title "Experience"
                                                                       :name  "experience"
                                                                       :group :character}
                                                                      {:title "Upbringing"
                                                                       :name  "upbringing"
                                                                       :group :character}
                                                                      {:title "Gift"
                                                                       :name  "gift"
                                                                       :group :character}
                                                                      {:title "Mark"
                                                                       :name  "mark"
                                                                       :group :character}
                                                                      {:title "Bond"
                                                                       :name  "bond"
                                                                       :group :character}
                                                                      {:title "Charm"
                                                                       :name  "charm"
                                                                       :group :character}]
                                                    :word-bank-count 2
                                                    :generators      generators})
                          (character-sheets/initial-state {:name-key   :name
                                                           :players    players
                                                           :lockable?  false
                                                           :intro-card dossier-template})
                          (prompt-deck/initial-state {:features {:everyone true}
                                                      :deck     (build-draw-deck decks
                                                                                 {:mission-details mission-details
                                                                                  :scene-count     11})}))

        new-game (merge
                  initial-state
                  {:game-type     :push
                   ;; TODO: better matrix handling for state
                   :matrix        (first (:matrix mission-details))
                   $decks         decks
                   :ready-players {}})]
    (render-game-display new-game)))

(defn extract-dossier [{:keys [inputs]}]
  (zipmap (map keyword (map :name inputs))
          (map :value inputs)))

(defn finish-card [game]
  (let [active-player    (player-order/active-player game)
        next-state       (-> game
                             player-order/activate-next-player!
                             word-bank/regen-word-banks!
                             x-card/reset-x-card!
                             prompt-deck/draw-next-card!
                             (undoable/checkpoint! game))]
    (render-game-display next-state)))

(defn draw-new-encounter [{current-matrix :matrix
                           :as game}]
  (let [matrix (get-in game [:mission-details :matrix])
        next-matrix (if current-matrix
                      (first (rest (drop-while #(not= % current-matrix) matrix)))
                      (first matrix))]
    (if next-matrix
      (assoc game
             :matrix next-matrix
             :challenge (word-bank/->pluck game "challenge"))
      game)))

(defn next-phase [game]
  (let [active-player    (player-order/active-player game)
        next-state       (-> game
                             player-order/activate-next-phase!
                             game-stages/next-phase!
                             word-bank/regen-word-banks!
                             x-card/reset-x-card!)
        next-state       (if (= :encounter (game-stages/->phase next-state))
                           (draw-new-encounter next-state)
                           next-state)]
    (render-game-display (undoable/checkpoint! next-state game))))

(defn discard-card [game]
  (let [next-game       (-> game
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


;; TODO: Fix how readiness is marked
(defn change-stage [uid {:keys [stage]} {:keys [ready-players] :as game}]
  (let [player-order  (player-order/player-order game)
        ready-players (assoc ready-players uid true)]
    (if (some #(nil? (ready-players %)) (map :id player-order))
      (assoc-in game [:ready-players] ready-players)
      (-> game
          (game-stages/change-stage! stage)
          (assoc :ready-players {})
          (undoable/checkpoint! game)
          render-game-display))))

;; TODO: How much stress does this add to duratom?
(defn undo-card [game]
  (let [{:keys [-last-state]} game
        new-state (undoable/undo! game)]
    (render-game-display new-state)))

(defn x-card [game]
  (-> (x-card/activate-x-card! game)
      (undoable/checkpoint! game)
      render-game-display))

(defn end-game [game]
  nil)

(defn tick-clock [game]
  ;; Nothing
  game)

(defn regen-card [uid
                  params
                  {:keys [stage]
                   :as   game}]
  (-> game
      (character-sheets/regen! {:id uid} params)
      render-game-display))

(defn consult [params
               {:keys [stage]
                :as   game}]
  (-> game
      (oracle-box/consult! (assoc params :replace-vars replace-vars))
      render-game-display))

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
                        :next-phase      next-phase
                        ;; :ready           (partial make-ready uid)
                        :change-stage    (partial change-stage uid params)
                        :regen           (partial regen-card uid params)
                        :consult         (partial consult params)
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
