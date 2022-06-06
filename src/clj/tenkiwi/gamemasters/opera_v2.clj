(ns tenkiwi.gamemasters.opera-v2
  "This is the game logic for Breathless investigation games"
  (:require
   [clojure.string :as string]
   [tenkiwi.util :as util :refer [inspect push-uniq]]
   [tenkiwi.rules.player-order :as player-order]
   [tenkiwi.rules.prompt-deck :as prompt-deck]
   [tenkiwi.rules.x-card :as x-card]
   [tenkiwi.rules.word-bank :as word-bank]
   [tenkiwi.rules.stress :as stress]
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
  (let [next-player                     (:id (player-order/next-player game))
        prev-player                     (:id (player-order/previous-player game))
        player-names                    (character-sheets/->player-names game)
        {:keys [complications clues
                opportunities hazards]} episode
        setup                           (:setup episode)]
    (merge
     setup
     {:clue         (nth clues scene-number (rand-nth clues))
      :hazard       (nth hazards scene-number (rand-nth hazards))
      :opportunity  (nth opportunities scene-number (rand-nth opportunities))
      :player-left  (get-in player-names [prev-player] "")
      :player-right (get-in player-names [next-player] "")
      :opening      (:text episode)
      :complication (nth complications scene-number (first complications))})))

(defn replace-vars [variables str-or-card]
  (let [text      (if (string? str-or-card)
                    str-or-card
                    (:text str-or-card))
        replaced  (string/replace (or text "")
                                  #"\{([^\}]+)\}"
                                  #(get variables (keyword (nth % 1))
                                        (nth % 1)))
        ;; Recursives
        replaced  (string/replace (or replaced "")
                                  #"\{([^\}]+)\}"
                                  #(get variables (keyword (nth % 1))
                                        (nth % 1)))]
    (cond
      (string? str-or-card)
      replaced
      (map? str-or-card)
      (assoc str-or-card :text replaced)
      :else str-or-card)))

(defn replace-text [game str-or-card]
  (replace-vars (extract-vars game) str-or-card))

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


(defn one-per-number [act-collection]
  (let [grouped (group-by :number act-collection)]
    (zipmap (keys grouped)
            (map first (vals grouped)))))

(defn one-per-concept [act-collection]
  (let [grouped (group-by :concept act-collection)]
    (zipmap (keys grouped)
            (map first (vals grouped)))))

;; TODO: Fix build act to match planned algo
;; - need to mix enough clues into acts 1, 2 (does act 3 get any - would just help position)
;; - placeholders for other prompts
(defn build-act [{:keys [prompt]
                  :as   decks}
                 episode
                 scene-count
                 act-number]
  (let [;; Maybe add challenge && loose end
        types   (cond
                  (= act-number 0)
                  [:complication :clue :clue :clue :clue :hazard :hazard :opportunity :opportunity]
                  (= act-number 1)
                  [:complication :complication :clue :clue :hazard :hazard :opportunity]
                  :else
                  [:complication :complication :hazard :hazard]
                  )
        ;; Make this more dynamic
        scenes  (cond
                  (= act-number 0)
                  [:interrogation :exploration :research :confrontation]
                  (= act-number 1)
                  [:research :interrogation :exploration :confrontation]
                  :else
                  [:research :exploration :confrontation :confrontation])
        fillers (repeat {:type       :prompt
                         ;; :concept    concept
                         :filler?    true
                         :text       "filler"
                         :number     (str act-number)
                         :act-number act-number})]
    (->> (map
          (fn [scene-num scene-type]
            (map #(assoc % :scene-number scene-num)
                 (concat
                  [(assoc (first fillers)
                          :text "scene open"
                          :required-tags [scene-type]
                          :concept (:concept episode)
                          :type :scene)]
                  (map #(assoc % :concept (name scene-type))
                       (take 2 fillers))
                  [(assoc (first fillers)
                          :concept (name scene-type)
                          :text "scene close"
                          :type (rand-nth types))]
                  )))
          (range scene-count)
          (take scene-count scenes))
         (interpose [{:type :scene-change
                      :text "**Scene change**\n\nTake a moment to write down any notes about this scene and any possible clues gathered."}])
         (apply concat)
         (into []))))

(defn build-draw-deck [{intro-cards :intro
                        prompts     :prompt
                        scenes      :scene
                        :as         decks}
                       {:keys [episode
                               players]}]
  (let [concepts  (group-by :concept scenes)
        scenes    (concat (get concepts "any")
                          (get concepts (:concept episode))) ;; Allow generic and concept specific scenes
        act-count (count (one-per-number scenes))]
    (into []
          (concat intro-cards
                  ;; TODO: extract texts
                  [{:type :opening :text "{opening}"}
                   {:type :act-change
                    :text "**Act 1**\n\nInitial investigations - think about where you might start with the info you have."}]
                  (build-act decks episode 4 0)
                  [{:type :act-change :tags {:increment-clock "doom"}
                    :text "**Act 2**\n\nDanger is accelerating. If you encountered a loose end, one come back to bite you now - roll to resolve it. Otherwise, you can stop to catch your breath at the cost of 1 additional doom."}]
                  (build-act decks episode 4 1)
                  [{:type :act-change :text "**Act 3**\n\nIt's time to act. Make a theory about what might be happening and how to contain it, the make a containment roll!"}]
                  (build-act decks episode 4 2)
                  [{:type :ending :text "**Epilogue**\n\nThis case has come to a close. Have any loose ends been left off? How do the survivors deal with the aftermath?"}]
                  #_(mapcat #(build-round % card-count decks)
                          (keys act-names))
                  #_[(:ending-card mission-details)]))))

(defn prepare-episode [{episode    :episode
                        options    :options
                        generators :generator
                        :as        decks}
                       {:keys [concept]}]
  (let [random  (:number (first (shuffle episode)))
        gen     (partial util/pluck-text (group-by :concept generators))
        opening (-> (one-per-number episode)
                    (get (or concept random)))

        ;; TODO - crawl and find vars / add param?
        episode-setup (case (get opening :concept "magick")
                        "magick"
                        {:magick-location (gen "magick-location")
                         :magick-victim   (gen "magick-victim")
                         :magick-sign     (gen "magick-sign")}
                        ;;else
                        {})

        options (-> (one-per-concept options)
                    (util/update-keys keyword)
                    (util/update-values :text))
        ]
    (-> opening
        (update :text (partial replace-vars episode-setup))
        (update :complications #(->> (string/split % #"\s\s+") (map string/trim)))
        (update :challenges #(->> (string/split % #"\s\s+") (map string/trim)))
        (update :clues #(->> (string/split % #"\s\s+") (map string/trim)))
        (update :hazards #(->> (string/split % #"\s\s+") (map string/trim)))
        (update :opportunities #(->> (string/split % #"\s\s+") (map string/trim)))
        (assoc :setup episode-setup)
        (assoc :options options))))

(defn render-stage-info [{:keys [company
                                 act-names
                                 focus-names
                                 scene-number]
                          :as   game}]
  (let [next-card       (prompt-deck/active-card game)
        next-stage      (get next-card :type :intro)
        next-scene-number (:scene-number next-card)]
    (-> game
        (update-in [:display :card] (partial replace-text game))
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
      clock-list/render-scoreboard-display
      dice-bag/render-dice-bag-display
      render-stage-info
      render-active-display
      render-inactive-display))

(defn select-game [room-id {:keys [game-url concept]
                            :as   params
                            :or   {shifts  0
                                   concept :random}}
                   {:keys [players] :as room}]
  (let [{:keys [episode]} (util/gather-decks game-url)
        sheet-template    {:nickname "Nickname"}
        new-game          {:game-type      :opera-v2
                           :configuration  {:inputs [{:type    :select
                                                      :label   "Pick an Episode"
                                                      :name    :concept
                                                      :options (concat [{:value :random :name "Random"}]
                                                                       (mapv #(hash-map
                                                                               :value (:number %)
                                                                               :name (-> (:text %) (clojure.string/split #"\s\s+") first))
                                                                             episode))}]}
                           :sheet-template sheet-template}]
    new-game))

(defn start-game [room-id {:keys [game-url concept]
                           :or   {}}
                  {:keys [players] :as room}]
  (let [decks      (util/gather-decks game-url)
        generators (->> decks :generator (group-by :concept))
        episode    (prepare-episode decks {:concept (if (string? concept) concept)})

        sheet-template {:text "Stuff happens." :inputs "Foo: bar"}

        initial-state (-> {:game-type :opera-v2
                           :epsiode   episode}
                          (player-order/initial-state room)
                          (x-card/initial-state {})
                          (character-sheets/initial-state {:name-key   :nickname
                                                           :intro-card sheet-template
                                                           :players    players})
                          (stress/initial-state {:players players})
                          (undoable/initial-state {:skip-keys [:display :active-display :inactive-display]})
                          (word-bank/initial-state {:word-banks    (get-in episode [:options :word-banks])
                                                    :word-bank-key :extra-details
                                                    :generators    generators})
                          (clock-list/initial-state {:allow-new? true
                                                     :clocks     [{:title    "Doom Track"
                                                                   :name     :doom
                                                                   :subtitle "**1-3** (1 danger) / **4-6** (2 danger) / **7-9** (3 danger)"
                                                                   :colors   {7 :red       8 :red       9 :red
                                                                              4 :goldenrod 5 :goldenrod 6 :goldenrod
                                                                              1 :green     2 :green     3 :green}
                                                                   :max      9
                                                                   :min      1
                                                                   :current  2}
                                                                  {:title    "Progress Clock"
                                                                   :subtitle (:success episode)
                                                                   :colors   {0 :blue      1 :blue      2 :blue
                                                                              3 :goldenrod 4 :goldenrod 5 :green
                                                                              6 :green     7 :green     8 :green}
                                                                   :max      8
                                                                   :current  0}
                                                                  {:title    "Danger Clock"
                                                                   :subtitle (:failure episode)
                                                                   :colors   {0 :yellow    1 :yellow    2 :goldenrod
                                                                              3 :goldenrod 4 :goldenrod 5 :orange
                                                                              6 :orange    7 :red       8 :red}
                                                                   :max      8
                                                                   :current  0}

                                                                  ]})
                          (dice-bag/initial-state {:shortcuts   [{:formula "1d4"
                                                                  :text    "d4"}
                                                                 {:formula "1d6"
                                                                  :text    "d6"}
                                                                 {:formula "1d8"
                                                                  :text    "d8"}
                                                                 {:formula "1d10"
                                                                  :text    "d10"}
                                                                 {:formula "1d12"
                                                                  :text    "d12"}
                                                                 {:formula "1d20"
                                                                  :text    "d20"}
                                                               ]
                                                   :input?      false
                                                   :breathless? true
                                                   :log         []})
                          (prompt-deck/initial-state {:features {:fillable? true}
                                                      :fillers  (select-keys decks [:prompt :scene :challenge :hazard :clue :complication :opportunity])
                                                      :deck     (build-draw-deck decks
                                                                                 {:episode episode
                                                                                  :players players})}))


        new-game (merge
                  initial-state
                  {:game-type    :opera-v2
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
        doom-val         (or (clock-list/->by-name game :doom) 5)
        doom-colors      (cond
                           (< 7 doom-val) [:red]
                           (< 5 doom-val) [:yellow :red]
                           (< 4 doom-val) [:yellow]
                           (< 2 doom-val) [:yellow :green]
                           :else [:green])

        next-state       (-> game
                             ;; Order matters - lock before transition player
                             character-sheets/maybe-lock-sheet!
                             player-order/activate-next-player!
                             word-bank/regen-word-banks!
                             x-card/reset-x-card!
                             prompt-deck/draw-next-card!
                             (prompt-deck/fill-card! {:tags    doom-colors
                                                      :number  true
                                                      :concept true})
                             (clock-list/update-by-tags! (:tags previous-card))
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
