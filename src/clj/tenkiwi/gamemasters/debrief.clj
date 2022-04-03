(ns tenkiwi.gamemasters.debrief
  "This is the game logic for debrief game"
  (:require
   [clojure.string :as string]
   [tenkiwi.tables.debrief :as tables]
   [tenkiwi.util :as util :refer [inspect]]
   [tenkiwi.rules.player-order :as player-order]))

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
          (for [stage stages
                rater (remove #{player-id} (keys ranks))]
            (cond
              (#{player-id} (get-in ranks [rater stage :best]))
              10
              (#{player-id} (get-in ranks [rater stage :worst]))
              3
              :else
              6)))))

(defn score-players [{:keys [dossiers player-ranks player-scores]}]
  (let [sum-scores  #(apply + (vals %))
        base-scores (zipmap (keys player-scores)
                            (map sum-scores (vals player-scores)))
        final-scores (reduce #(update %1 %2 + (score-ranks %2 player-ranks))
                             base-scores
                             (keys base-scores))]
    final-scores))

(defn extract-vars [{:keys [company
                            dossiers mission]
                     :as   game}]
  (let [next-player   (:id (player-order/next-player game))
        prev-player   (:id (player-order/previous-player game))
        secondaries   (:secondary-objectives mission [])
        complications (:complications mission [])
        scores        (score-players game)
        values        (:values company)]
    {:leader-name    (get-in game [:dossiers :leader :agent-codename] "")
     :player-left    (get-in game [:dossiers prev-player :agent-codename] "")
     :player-right   (get-in game [:dossiers next-player :agent-codename] "")
     :scoreboard     (string/join "\n"
                                (map #(str
                                       "* "
                                       (:agent-codename (second %))
                                       " - "
                                       (scores (first %)))
                                     dossiers))
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

(defn extract-generator-list [str]
  (->> (clojure.string/split str #"\s\s")
       (map #(clojure.string/split % #":\s+"))
       (map #(hash-map :name (first %)
                       :label (last %)))))

;; TODO: Use above thing
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
  [{:keys [user-name]}]
  {:id    "waiting"
   :type :inactive
   :text  (str "It is " user-name "'s turn...")})

(defn player-button [{:keys [dossiers]} params {:keys [id user-name]}]
  {:action  :rank-player
   :params  (merge params {:id id})
   :confirm true
   :text    (str (get-in dossiers [id :agent-codename] user-name))})

(defn upvote-current-player-button [{:keys [dossiers]} {:keys [id user-name]}]
  {:action  :upvote-player
   :params  {:player-id id}
   :text    (str "Upvote " (get-in dossiers [id :agent-codename] user-name))})

(defn downvote-current-player-button [{:keys [dossiers]} {:keys [id user-name]}]
  {:action  :downvote-player
   :params  {:player-id id}
   :text    (str "Downvote " (get-in dossiers [id :agent-codename] user-name))})


(defn build-active-card
  ([game card active-player next-player]
   (let [{:keys [all-players
                 mission
                 -generators]} game
         story-details         (extract-generator-list (:story-details mission ""))
         act                   (:act card)
         next-stage            (or (:type card) :intro)
         starting-player       (or (:starting-player card) active-player)
         ;; Don't allow done-action for #everyone cards until they are passed around
         can-finish?           (or (not (get-in card [:tags :everyone] false))
                                   (= (:starting-player card) active-player))
         pass                  {:action :pass
                                :text   (str "Pass card to " (:user-name next-player))}]
     {:card              (assoc (replace-vars game card)
                                :starting-player starting-player)
      :extra-details     (map #(hash-map :title (:label %)
                                     :name (:name %)
                                     :items (take 3 (shuffle (mapv :text (get -generators (:name %) [])))))
                          story-details)
      :extra-actions     [undo-action leave-game-action]
      :available-actions valid-active-actions
      :actions           (case next-stage
                           :end        [pass end-game-action]
                           :upvoting   (mapv (partial player-button game {:rank :best
                                                                          :act  act}) all-players)
                           :downvoting (mapv (partial player-button game {:rank :worst
                                                                          :act  act}) all-players)
                           :dossier    [done-action]
                           (if can-finish?
                             [done-action pass]
                             [pass]))}))
  ([card active-player next-player]
   (build-active-card {} card active-player next-player)))

(defn build-inactive-card [active-player extra-text]
  (let [waiting (waiting-for active-player)
        waiting (if extra-text
                  (update waiting
                          :text
                          (partial str extra-text "\n\n"))
                  waiting)]

    {:card          waiting
     :available-actions valid-inactive-actions
     :extra-actions [undo-action leave-game-action]}))

(defn build-inactive-version [{:keys [dossiers active-player] :as game}
                              {{:keys [type]} :card
                               :as            active-display}]
  (let [disabled-actions [{:text      (:text (waiting-for active-player))
                           :disabled? true}]
        extra-actions    [(upvote-current-player-button dossiers active-player)
                          (downvote-current-player-button dossiers active-player)]]
    (cond
      (#{:act-start :question :intro :mission-briefing} type)
      (-> active-display
          (assoc :available-actions valid-inactive-actions)
          (assoc :actions disabled-actions)
          (update :actions #(into % extra-actions)))
      (#{:dossier} type)
      (build-inactive-card active-player "Introductions are being made.")
      :else
      (-> active-display
          (assoc :available-actions valid-inactive-actions)))))

(defn- build-starting-scores [{:keys [npc? id]} players]
  (let [ids (remove #(= id %) (map :id players))]
    (zipmap ids (cycle [5]))))

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

(defn pluck
  ([generators gen-name]
   (first (pluck generators gen-name 1)))
  ([generators gen-name count]
   (->> [{:text "Foo"}]
        (get generators gen-name)
        shuffle
        (take count)
        (map :text))))

(defn start-game [room-id {:keys [game-url]
                           :or   {}}
                  {:keys [players] :as room}]
  (let [order-state         (player-order/initial-state room)
        first-player        (player-order/active-player order-state)
        next-player         (player-order/next-player order-state)
        npcs                [{:user-name "NPC"
                              :id        :leader
                              :dead?     true
                              :npc?      true}]
        {intro-cards :intro
         questions   :question
         missions    :mission
         :as         decks} (util/gather-decks game-url)
        act-names           (-> decks :act-name (one-per-act :text))
        focus-names         (-> decks :focus-name (one-per-act :text))
        mission-briefing    (->> decks :mission-briefing (group-by :act))
        generators          (->> decks :generator (group-by :act))
        dossier-template    (->> decks :dossier first)
        mission-details     (build-mission-details mission-briefing (first (shuffle missions)))

        all-players (concat (into [] players)
                               npcs)
        card-count  11
        company     {:name   (pluck generators "company")
                     :values (pluck generators "value" 3)}
        dossiers    {:leader {:agent-name     (tables/random-name)
                              :agent-codename (pluck generators "leader-codename")
                              :agent-role     (pluck generators "leader-role")}}

        active-display (build-active-card (first intro-cards) first-player next-player)
        new-game       (merge
                        order-state
                        {:player-scores    (into {}
                                                 (map #(vector (:id %)
                                                               (build-starting-scores % players)) all-players))
                         :player-ranks     (zipmap
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
                         :dossiers         dossiers
                         :mission          mission-details
                         :-discard          []
                         :company          company
                         :dossier-template dossier-template
                         :-generators       generators
                         :-deck             (into []
                                                  (concat (rest intro-cards)
                                                          (map (partial dossier-card dossier-template generators) players)
                                                          (:briefing-cards mission-details)
                                                          (mapcat #(build-round % card-count decks)
                                                                  (keys act-names))
                                                          [(:ending-card mission-details)]))
                         :active-display   active-display
                         :inactive-display (build-inactive-version {:active-player first-player} active-display)})]
    new-game))

(defn extract-dossier [{:keys [inputs]}]
  (zipmap (map keyword (map :name inputs))
          (map :value inputs)))

(defn get-stage-info [{:keys [company
                              act-names
                              stage-names
                              focus-names]
                       :as game}
                      next-card]
  (let [next-stage      (get next-card :type :intro)
        next-act        (clojure.string/replace (str (get next-card :act "0"))
                                                #"[^\d]"
                                                "")
        next-stage-name (-> (get stage-names next-stage "Introduction")
                            (clojure.string/replace #"\{act-name\}" (get act-names next-act)))

        next-stage-focus (cond (#{:question :act-start} next-stage)
                               (replace-vars game (get focus-names next-act (str "{value-" next-act "}")))
                               :else "")]
    {:stage       next-stage
     :stage-name  next-stage-name
     :stage-focus next-stage-focus}))

(defn finish-card [game]
  (let [{:keys [dossiers
                -discard
                -deck
                stage]}  game
        active-player    (player-order/active-player game)
        next-state       (-> game
                             (player-order/activate-next-player!))
        active-card      (get-in game [:active-display :card])
        dossiers         (if (#{:player-dossier} (:id active-card))
                          (assoc dossiers (:id active-player)
                                 (extract-dossier active-card))
                          dossiers)
        next-up          (player-order/active-player next-state)
        discard          (cons active-card -discard)
        next-card        (first -deck)
        deck             (into [] (rest -deck))
        stage-info       (get-stage-info game next-card)
        next-next        (player-order/next-player next-state)

        next-game          (assoc next-state
                                  :-deck deck
                                  :dossiers dossiers
                                  :-discard discard
                                  :-last-state game)
        new-active-display (build-active-card next-game next-card next-up next-next)]
    (-> next-game
        (merge stage-info)
        (assoc
         :active-display new-active-display
         :inactive-display (build-inactive-version next-game new-active-display)))))

(defn discard-card [game]
  (let [{:keys [-discard
                -deck
                stage]} game
        active-card     (get-in game [:active-display :card])
        active-player   (player-order/active-player game)
        next-up         (player-order/next-player game)
        discard         (cons active-card -discard)
        next-card       (first -deck)
        deck            (rest -deck)
        stage-info      (get-stage-info game next-card)

        next-game          (-> game
                            (assoc-in [:inactive-display :x-card-active?] false)
                            (assoc :-deck deck
                                   :-last-state game
                                   :-discard discard))
        new-active-display (build-active-card next-game next-card active-player next-up)]
    (assoc (merge next-game stage-info)
           :active-display new-active-display
           :inactive-display (build-inactive-version next-game new-active-display))))


(defn pass-card [game]
  (let [next-state              (player-order/activate-next-player! game)
        active-card             (get-in game [:active-display :card])
        next-up                 (player-order/active-player next-state)
        next-next               (player-order/next-player next-state)
        next-game               (assoc next-state
                                       :-last-state game)
        new-active-display      (build-active-card next-game active-card next-up next-next)]
    (assoc next-game
           :inactive-display (build-inactive-version next-game new-active-display)
           :active-display new-active-display)))

;; TODO: How much stress does this add to duratom?
(defn undo-card [game]
  (let [{:keys [player-scores
                -last-state]} game]
    (assoc -last-state
           :player-scores
           (get-in game [:player-scores]))))

(defn push-uniq [coll item]
  (if (some #(= % item) coll)
    coll
    (into [item] coll)))

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
   {:keys [all-players player-scores dossiers] :as game}]
  (let [current-score (get-in player-scores [player-id voter-id])
        new-score (min (inc current-score) 10)
        player-names (group-by :id all-players)
        player-name (or (get-in dossiers [player-id :agent-codename])
                        (get-in player-names [player-id 0 :user-name] "Someone"))]
    (if current-score
      (-> game
          (assoc-in [:player-scores player-id voter-id] new-score)
          (assoc-in [:broadcasts] [[:->toast/show! (str "👍  " player-name " got upvoted.")]]))
      game)))

(defn downvote-player
  [voter-id
   {:keys [player-id]}
   {:keys [all-players player-scores dossiers] :as game}]
  (let [current-score (get-in player-scores [player-id voter-id])
        new-score (max (dec current-score) 0)
        player-names (group-by :id all-players)
        player-name (or (get-in dossiers [player-id :agent-codename])
                        (get-in player-names [player-id 0 :user-name] "Someone"))]
    (if current-score
      (-> game
          (assoc-in [:player-scores player-id voter-id] new-score)
          (assoc-in [:broadcasts] [[:->toast/show! (str "👎  " player-name " got downvoted.")]]))
      game)))

(defn x-card [game]
  (let [{:keys []} game]
    (-> game
        (assoc-in [:active-display :x-card-active?] true)
        (update-in [:active-display :actions] push-uniq discard-action)
        (assoc-in [:inactive-display :x-card-active?] true))))

(defn end-game [game]
  nil)

(defn tick-clock [game]
  ;; Nothing
  game)

(defn regen-card [params
                  {:keys [-generators dossier-template stage]
                   :as   game}]
  (let [next-up       (player-order/next-player game)
        active-player (player-order/active-player game)
        next-dossier  (build-active-card game
                                        (dossier-card dossier-template -generators active-player params)
                                        active-player
                                        next-up)]
    (cond
      (#{:dossier} stage)
      (-> game
          (assoc :active-display next-dossier))
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
