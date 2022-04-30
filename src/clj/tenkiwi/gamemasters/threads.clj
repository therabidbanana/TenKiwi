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
   [tenkiwi.rules.momentum :as momentum]
   [tenkiwi.rules.character-sheets :as character-sheets]
   [tenkiwi.rules.undoable :as undoable]
   [tenkiwi.rules.clock-list :as clock-list]
   [tenkiwi.rules.dice-bag :as dice-bag]
   ))

(def valid-active-actions #{:regen :pass :discard :undo :done :roll :x-card :end-game :score-player :downscore-player :upscore-player :upstress-player :downstress-player :increment-clock :decrement-clock :leave-game})
(def valid-inactive-actions #{:x-card :undo :leave-game :roll :upscore-player :downscore-player  :upstress-player :downstress-player :increment-clock :decrement-clock})

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

(defn extract-vars [{:keys [scene-number episode]
                     :as   game}]
  (let [next-player   (:id (player-order/next-player game))
        prev-player   (:id (player-order/previous-player game))
        player-names  (character-sheets/->player-names game)
        complications (:complications episode)
        outcomes      (str (:success episode) "\n\n" (:failure episode))]
    {:player-left    (get-in player-names [prev-player] "")
     :player-right   (get-in player-names [next-player] "")
     :outcomes       outcomes
     :opening        (:text episode)
     :complication   (nth complications scene-number (first complications))}))

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


(defn one-per-num [act-collection]
  (let [grouped (group-by :number act-collection)]
    (zipmap (keys grouped)
            (map first (vals grouped)))))

(defn one-per-concept [act-collection]
  (let [grouped (group-by :concept act-collection)]
    (zipmap (keys grouped)
            (map first (vals grouped)))))

(defn build-round [{:keys [prompt concept]
                    :as   decks}
                   round scene-number]
  (let [scene   (:scene round)
        concept (-> (one-per-concept concept)
                     (get scene))
        prompts (-> (group-by :concept prompt)
                    (get scene))
        starters (shuffle (take 6 prompts))
        enders   (shuffle (drop 6 prompts))]
    (into []
          (concat
           [(assoc concept
                   :scene-number scene-number)]
           (take 3 starters)
           (take 3 (shuffle (concat (drop 3 starters) enders)))))))

(defn rand-apply [do-to list x]
  (let [new-val (into [] (update list (rand-int (count list)) do-to))]
    new-val))

(defn roll-threads [{:keys [shifts]}]
  (let [max-inc (fn max-inc [current-score] (min (inc current-score) 6))
        min-dec (fn min-dec [current-score] (max (dec current-score) 1))
        shifter (if (> shifts 0)
                  max-inc
                  min-dec)
        dice    (reduce (partial rand-apply shifter)
                        (into [] (util/roll 9 6))
                        (range 0 (Math/abs shifts)))

        scenes (-> (mapv (partial reduce +)
                         (partition-all 3 dice)))
        total  (reduce + scenes)]
    {:dice   dice
     :total  total
     :scenes scenes}))

(defn build-draw-deck [{intro-cards :intro
                        openings    :opening
                        prompts     :prompt
                        scenes      :concept
                        :as         decks}
                       {:keys [episode
                               players
                               threads]}]
  (let [scenes     (one-per-concept scenes)
        scenes     (mapv #(-> (get scenes %) (assoc :scene %))
                       (map str (:scenes threads)))]
    (into []
          (concat intro-cards
                  [{:type :opening :text "**Episode Focus**\n\n{opening}"}
                   {:type :opening :text "Keep this episode's focus in mind as you answer the following prompts."}]
                  (->> (map #(build-round decks %1 %2)
                            scenes (range (count scenes)))
                       (interpose [{:type :complication
                                    :tags {:challenge true}
                                    :text "**Complication**\n\n{complication}"}])
                       (apply concat))
                  [{:type :ending :text "{outcomes}"}]
                  #_(mapcat #(build-round % card-count decks)
                          (keys act-names))
                  #_[(:ending-card mission-details)]))))

(defn prepare-episode [{openings :opening
                        options :options
                        :as      decks}
                       {:keys [threads episode]}]
  (let [opening           (-> (one-per-concept openings)
                              (get (or episode (str (:total threads)))))
        options          (-> (one-per-concept options)
                              (util/update-keys keyword)
                              (util/update-values :text))
        [success failure] (->> (string/split (:outcomes opening) #"\s\s+")
                               (map string/trim))]
    (-> opening
        (update :complications #(->> (string/split % #"\s\s+") (map string/trim)))
        (assoc :options options)
        (assoc :success success)
        (assoc :failure failure))))

(defn render-stage-info [{:keys [company
                                 act-names
                                 focus-names
                                 scene-number]
                          :as   game}]
  (let [next-card       (prompt-deck/active-card game)
        next-stage      (get next-card :type :intro)
        next-scene-number (:scene-number next-card)]
    (-> game
        (update-in [:display :card] (partial replace-vars game))
        (assoc
            :scene-number (or next-scene-number scene-number 0)
            :stage       next-stage))))

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
                                :ending     [pass end-game-action]
                                :dossier    [done-action]
                                [done-action pass])]
    (assoc game
           :active-display
           {:extra-actions     [undo-action leave-game-action]
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
             :else
             (-> active-display
                 (assoc :available-actions valid-inactive-actions))))))

(defn render-test [game]
  (println (keys game))
  (println (keys (:display game)))
  game)

(defn render-game-display [game]
  (-> game
      player-order/render-display
      prompt-deck/render-display
      x-card/render-display
      word-bank/render-display
      character-sheets/render-display
      stress/render-scoreboard-display
      momentum/render-scoreboard-display
      clock-list/render-scoreboard-display
      dice-bag/render-dice-bag-display
      render-stage-info
      render-active-display
      render-inactive-display))

(defn select-game [room-id {:keys [game-url shifts episode]
                            :as   params
                            :or   {shifts  0
                                   episode :random}}
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
                                                                       (mapv #(hash-map
                                                                               :value (:concept %)
                                                                               :name (-> (:text %) (clojure.string/split #"\s\s+") first))
                                                                             opening))}]}
                           :sheet-template sheet-template}]
    new-game))

(defn start-game [room-id {:keys [game-url shifts episode]
                           :or   {}}
                  {:keys [players] :as room}]
  (let [decks      (util/gather-decks game-url)
        generators (->> decks :generator (group-by :concept))
        ;; Hack shifts because I don't want to fix the lazy random
        threads    (roll-threads {:shifts (* 2 shifts)})
        episode    (prepare-episode decks {:threads threads
                                           :episode (if (string? episode) episode)})

        sheet-template {:text "Stuff happens." :inputs "Foo: bar"}

        initial-state (-> {:game-type :threads
                           :epsiode   episode
                           :threads   threads}
                          (player-order/initial-state room)
                          (x-card/initial-state {})
                          (character-sheets/initial-state {:name-key   :nickname
                                                           :intro-card sheet-template
                                                           :players    players})
                          (stress/initial-state {:players players})
                          (momentum/initial-state {:players players})
                          (undoable/initial-state {:skip-keys [:display :active-display :inactive-display]})
                          (word-bank/initial-state {:word-banks    (get-in episode [:options :word-banks])
                                                    :word-bank-key :extra-details
                                                    :generators    generators})
                          (clock-list/initial-state {:allow-new? true
                                                     :clocks     [{:title    "Progress Clock"
                                                                   :subtitle (:success episode)
                                                                   :colors   {0 :blue 1 :blue 2 :blue
                                                                              3 :goldenrod 4 :goldenrod 5 :green
                                                                              6 :green 7 :green 8 :green}
                                                                   :max      8
                                                                   :current  0}
                                                                  {:title    "Danger Clock"
                                                                   :subtitle (:failure episode)
                                                                   :colors   {0 :yellow 1 :yellow 2 :goldenrod
                                                                              3 :goldenrod 4 :goldenrod 5 :orange
                                                                              6 :orange 7 :red 8 :red}
                                                                   :max      8
                                                                   :current  0}
                                                                  {:title    "Position Track"
                                                                   :subtitle "**1-3** (3 danger) / **4-6** (2 danger) / **7-9** (1 danger)"
                                                                   :colors   {1 :red 2 :red 3 :red
                                                                              4 :goldenrod 5 :goldenrod 6 :goldenrod
                                                                              7 :green 8 :green 9 :green}
                                                                   :max      9
                                                                   :min      1
                                                                   :current  5}
                                                                  ]})
                          (dice-bag/initial-state {:shortcuts [{:formula "1d6"
                                                                :text "1"}
                                                               {:formula "2d6"
                                                                :text "2"}
                                                               {:formula "3d6"
                                                                :text "3"}
                                                               {:formula "4d6"
                                                                :text "4"}]
                                                   :input? false
                                                   :charge? true
                                                   :log []})
                          (prompt-deck/initial-state {:features {}
                                                      :deck     (build-draw-deck decks
                                                                                 {:threads threads
                                                                                  :episode episode
                                                                                  :players players})}))


        new-game (merge
                  initial-state
                  {:game-type    :threads
                   :episode      episode
                   :players      players
                   :scene-number 0
                   :stage        :intro})]
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

(defn upscore-player
  [voter-id
   {:keys [player-id]}
   game]
  (let [current-score (momentum/current-score game [player-id])
        player-name   (-> (character-sheets/->player-names game)
                          (get player-id))]
    (if current-score
      (-> (momentum/upscore! game [player-id])
          render-game-display)
      game)))

(defn downscore-player
  [voter-id
   {:keys [player-id]}
   game]
  (let [current-score (momentum/current-score game [player-id])
        player-name   (-> (character-sheets/->player-names game)
                          (get player-id))]
    (if current-score
      (-> (momentum/downscore! game [player-id])
          render-game-display)
      game)))

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

(defn increment-clock
  [{:as params :keys [clock-id]}
   game]
  (-> (clock-list/increment-clock! game params)
      render-game-display))

(defn decrement-clock
  [{:as params :keys [clock-id]}
   game]
  (-> (clock-list/decrement-clock! game params)
      render-game-display))

(defn roll
  [{:as params :keys [formula]}
   game]
  (-> (dice-bag/roll! game params)
      render-game-display))

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
                        :increment-clock   (partial increment-clock params)
                        :decrement-clock   (partial decrement-clock params)
                        :roll              (partial roll params)
                        :upstress-player   (partial upstress-player uid params)
                        :downstress-player (partial downstress-player uid params)
                        :upscore-player    (partial upscore-player uid params)
                        :downscore-player  (partial downscore-player uid params)

                        ;; This is actual timekeeper clock. Ignore.
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
