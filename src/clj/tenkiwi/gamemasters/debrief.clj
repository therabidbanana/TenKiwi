(ns tenkiwi.gamemasters.debrief
  "This is the game logic for debrief game"
  (:require
   [clojure.string :as string]
   [tenkiwi.tables.debrief :as tables]
   [tenkiwi.util :as util :refer [inspect]]))

(def valid-active-actions #{:rank-player :regen :pass :discard :done :x-card :end-game :upvote-player :downvote-player :leave-game})
(def valid-inactive-actions #{:rank-player :x-card :undo :leave-game :upvote-player :downvote-player})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))

(defn previous-player [player-order current-player]
  (let [curr-id    (:id current-player)
        curr-index (.indexOf (mapv :id player-order) curr-id)
        prev-index (dec curr-index)
        prev-index (if (> 0 prev-index)
                     (dec (count player-order))
                     prev-index)]
    (nth player-order prev-index)))

(defn next-player [player-order current-player]
  (let [curr-id    (:id current-player)
        curr-index (.indexOf (mapv :id player-order) curr-id)
        next-index (inc curr-index)
        next-index (if (>= next-index (count player-order))
                     0
                     next-index)]
    (nth player-order next-index)))

(def done-action
  {:action :done
   :text   "Finish Turn"})

(def regen-action
  {:action :regen
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

(defn company-values-card [{:keys [values]}]
  {:id :values
   :type :intro
   :text (str "Always remember our organization's core values:\n\n* "
              (clojure.string/join "\n* " values))})

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

(defn extract-vars [{:keys [active-player player-order company
                            dossiers mission]
                     :as   game}]
  (let [next-player (:id (next-player player-order active-player))
        prev-player (:id (previous-player player-order active-player))
        secondaries (:secondary-objectives mission [])
        scores      (score-players game)
        values      (:values company)]
    {:leader-name  (get-in game [:dossiers :leader :agent-codename] "")
     :player-left  (get-in game [:dossiers prev-player :agent-codename] "")
     :player-right (get-in game [:dossiers next-player :agent-codename] "")
     :scoreboard   (string/join "\n"
                                (map #(str
                                       "* "
                                       (:agent-codename (second %))
                                       " - "
                                       (scores (first %)))
                                     dossiers))
     :value-0      (nth values 0)
     :value-1      (nth values 1)
     :value-2      (nth values 2)
     :primary      (:primary-objective mission)
     :secondary    (first (shuffle secondaries))
     ;; :secondary-0  (nth secondaries 0)
     ;; :secondary-1  (nth secondaries 1)
     ;; :secondary-2  (nth secondaries 2)
     ;; :secondary-3  (nth secondaries 3)
     ;; :secondary-4  (nth secondaries 4)
     ;; :secondary-5  (nth secondaries 5)
     }))

(defn replace-vars [game str-or-card]
  (let [text      (if (string? str-or-card)
                    str-or-card
                    (:text str-or-card))
        game-vars (extract-vars game)
        replaced  (string/replace (or text "")
                                          #"\{(.+)\}"
                                          #(get game-vars (keyword (nth % 1))
                                                (nth % 1)))]
    (cond
      (string? str-or-card)
      replaced
      (map? str-or-card)
      (assoc str-or-card :text replaced)
      :else str-or-card)))

(defn dossier-card [{:keys []}]
  (let [random-name     (tables/random-name)
        random-codename (tables/random-codename)
        random-skill    (tables/random-skill)]
    {:id     :player-dossier
     :type   :dossier
     :text   "Take a moment to introduce your character to the rest of the group.  Tell us their name, specialty and maybe add in a fun fact about them."
     :inputs [{:name      "agent-name"
               :label     "Agent Name"
               :value     random-name
               :generator :name}
              {:name      "agent-codename"
               :label     "Agent Codename"
               :value     random-codename
               :generator :codename}
              {:name      "agent-role"
               :label     "Team Role"
               :value     random-skill
               :generator :skill}]}))

(defn best-voting-round-card [act]
  {:id   (str "upvoting-" act)
   :act  act
   :type :upvoting
   :text (-> "Which agent best exemplified the company value of VALUE during the mission? Take some time to discuss, then pick one of the options."
              (string/replace #"VALUE" (str "{value-" act "}")))})

(defn worst-voting-round-card [act]
  {:id   (str "downvoting-" act)
   :act  act
   :type :downvoting
   :text (-> "Which agent least exemplified the company value of VALUE during the mission? Take some time to discuss, then pick one of the options."
              (string/replace #"VALUE" (str "{value-" act "}")))})

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

(defn build-active-card
  ([game card active-player next-player]
   (let [{:keys [all-players]} game
         act                   (:act card)
         next-stage            (or (:type card) :intro)
         pass                  {:action :pass
                                :text   (str "Pass card to " (:user-name next-player))}]
     {:card          (replace-vars game card)
      :extra-actions (case next-stage
                       :end        [leave-game-action]
                       [leave-game-action])
      :actions       (case next-stage
                       :end        [pass end-game-action]
                       :upvoting   (mapv (partial player-button game {:rank :best
                                                                      :act  act}) all-players)
                       :downvoting (mapv (partial player-button game {:rank :worst
                                                                      :act  act}) all-players)
                       :dossier    [regen-action done-action]
                       [done-action pass])}))
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
     :extra-actions [leave-game-action]}))

(defn build-inactive-version [{:keys [active-player] :as game}
                              {{:keys [type]} :card
                               :as            active-display}]
  (let [disabled-actions [{:text      (:text (waiting-for active-player))
                           :disabled? true}]]
    (cond
      (#{:act-start :question :intro :mission-briefing} type)
      (-> active-display
          (assoc :actions disabled-actions))
      (#{:dossier} type)
      (build-inactive-card active-player "Introductions are being made.")
      :else
      active-display)))

(defn- build-starting-scores [{:keys [npc? id]} players]
  (let [ids (remove #(= id %) (map :id players))]
    (zipmap ids (cycle [5]))))

(def mission-briefing
  [{:type :mission-briefing
    :text "Let's take a moment to review the mission you were sent on. Please continue to read aloud."}])

(def protocol-reminder
  [{:type :mission-briefing
    :text "As you remember from training, all covert operations can be divided into three basic stages:\n\n1. *On the case* - getting prepared and into position.\n2. *Getting in* - infiltration and achievement of main objective.\n3. *Getting out* - removing yourself from the situation without a trace."}])

(defn build-mission-details [{:keys [text secondary-objectives complications]
                        :as card}]
  (let [briefing (->> (string/split text #"\n\n")
                      (map #(hash-map :text % :type :mission-briefing)))]
    (-> card
        (assoc :briefing-cards (concat mission-briefing briefing protocol-reminder))
        (update :secondary-objectives #(string/split % #"\s\s")))))

(defn one-per-act
  ([act-collection]
   (let [grouped (group-by :act act-collection)]
     (zipmap (keys grouped)
             (map first (vals grouped)))))
  ([act-collection func]
   (let [grouped (group-by :act act-collection)]
     (zipmap (keys grouped)
             (map #(-> % first func) (vals grouped))))))

(defn start-game [world-atom room-id]
  (let [players      (get-in @world-atom [:rooms room-id :players])
        first-player (first players)
        next-player  (next-player players (:id first-player))
        npcs         [{:user-name "NPC"
                       :id        :leader
                       :dead?     true
                       :npc?      true}]
        dossiers     {:leader {:agent-name     (tables/random-name)
                               :agent-codename "Agent Pickles"
                               :agent-role     "Mission Leader"}}

        {intro-cards :intro
         questions   :question
         missions    :mission
         :as         decks} (util/gather-decks "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=1113383423&single=true&output=tsv")
        question-decks      (group-by :act questions)
        act-names           (-> decks :act-name (one-per-act :text))
        act-starts          (-> decks :act-start one-per-act)
        upvoting            (-> decks :upvoting one-per-act)
        downvoting          (-> decks :downvoting one-per-act)
        mission-details     (build-mission-details (first (shuffle missions)))

        all-players    (concat (into [] players)
                               npcs)
        card-count     11
        company        {:name   "VISA"
                        :values (take 3 (shuffle tables/company-values))}
        active-display (build-active-card (first intro-cards) first-player next-player)
        new-game       {:player-order     (into [] players)
                        :player-scores    (into {}
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
                        :stage            :intro
                        :stage-name       "Introduction"
                        :stage-focus      ""
                        :dossiers         dossiers
                        :mission          mission-details
                        :discard          []
                        :company          company
                        :deck             (into []
                                                (concat (rest intro-cards)
                                                        [(company-values-card company)]
                                                        (map dossier-card players)
                                                        (:briefing-cards mission-details)
                                                        [(get act-starts "0")]
                                                        (take card-count (shuffle (question-decks "0")))
                                                        [(get upvoting "0") (get downvoting "0")]
                                                        [(get act-starts "1")]
                                                        (take card-count (shuffle (question-decks "1")))
                                                        [(get upvoting "1") (get downvoting "1")]
                                                        [(get act-starts "2")]
                                                        (take card-count (shuffle (question-decks "2")))
                                                        [(get upvoting "2") (get downvoting "2")]
                                                        [{:type :end
                                                          :text "{scoreboard}"}]))
                        :active-player    (first players)
                        :active-display   active-display
                        :inactive-display (build-inactive-version {:active-player (first players)} active-display)}]
    (doto world-atom
      (swap! update-in [:rooms room-id] assoc :game new-game))))

(defn extract-dossier [{:keys [inputs]}]
  (zipmap (map keyword (map :name inputs))
          (map :value inputs)))

(defn get-stage-info [{:keys [company act-names stage-names]} next-card]
  (let [next-stage      (get next-card :type :intro)
        next-act        (str (get next-card :act "0"))
        next-stage-name (-> (get stage-names next-stage "Introduction")
                            (clojure.string/replace #"\{act-name\}" (get act-names next-act)))

        next-stage-focus (cond (#{:question :act-start} next-stage)
                               (nth (:values company) (Integer/parseInt next-act) )
                               :else "")]
    {:stage       next-stage
     :stage-name  next-stage-name
     :stage-focus next-stage-focus}))

(defn finish-card [game]
  (let [{:keys [player-order
                active-player
                dossiers
                discard
                deck
                stage]}  game
        active-card      (get-in game [:active-display :card])
        dossiers         (if (#{:player-dossier} (:id active-card))
                          (assoc dossiers (:id active-player)
                                 (extract-dossier active-card))
                          dossiers)
        next-up          (next-player player-order active-player)
        discard          (cons active-card discard)
        next-card        (first deck)
        deck             (into [] (rest deck))
        stage-info       (get-stage-info game next-card)
        next-next        (next-player player-order next-up)

        next-game          (assoc game
                                  :deck deck
                                  :dossiers dossiers
                                  :discard discard
                                  :active-player next-up)
        new-active-display (build-active-card next-game next-card next-up next-next)]
    (-> next-game
        (merge stage-info)
        (assoc
         :active-display new-active-display
         :inactive-display (build-inactive-version next-game new-active-display)))))

(defn discard-card [game]
  (let [{:keys [player-order
                active-player
                discard
                deck
                stage]} game
        active-card     (get-in game [:active-display :card])
        next-up         (next-player player-order active-player)
        discard         (cons active-card discard)
        next-card       (first deck)
        deck            (rest deck)
        stage-info      (get-stage-info game next-card)

        next-game          (-> game
                            (assoc-in [:inactive-display :x-card-active?] false)
                            (assoc :deck deck
                                   :discard discard))
        new-active-display (build-active-card next-game next-card active-player next-up)]
    (assoc (merge next-game stage-info)
           :active-display new-active-display)))


(defn pass-card [game]
  (let [{:keys [player-order
                active-player]} game
        active-card             (get-in game [:active-display :card])
        next-up                 (next-player player-order active-player)
        next-next               (next-player player-order next-up)
        next-game               (assoc game :active-player next-up)
        new-active-display      (build-active-card next-game active-card next-up next-next)]
    (assoc next-game
           :inactive-display (build-inactive-version next-game new-active-display)
           :active-display (inspect new-active-display))))

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
   {:keys [player-scores] :as game}]
  (let [current-score (get-in player-scores [player-id voter-id])
        new-score (min (inc current-score) 10)]
    (if current-score
      (-> game
         (assoc-in [:player-scores player-id voter-id] new-score))
      game)))

(defn downvote-player
  [voter-id
   {:keys [player-id]}
   {:keys [player-scores] :as game}]
  (let [current-score (get-in player-scores [player-id voter-id])
        new-score (max (dec current-score) 0)]
    (if current-score
      (-> game
          (assoc-in [:player-scores player-id voter-id] new-score))
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

(defn regen-card [{:keys [active-player player-order stage]
                   :as   game}]
  (let [next-up      (next-player player-order active-player)
        next-dossier (build-active-card game
                                        (dossier-card active-player)
                                        active-player
                                        next-up)]
    (cond
      (#{:dossier} stage)
      (-> game
          (assoc :active-display next-dossier))
      :else
      game)))

(defn if-active-> [uid action do-next-state]
  (fn [{:keys [active-player]
        :as game}]
    (let [active-player? (= (:id active-player) uid)]
      (if (valid-action? active-player? action)
        (do-next-state game)
        game))))

(defn take-action [world-atom {:keys [uid room-id action params]}]
  (let [game          (get-in @world-atom [:rooms room-id :game])
        do-next-state (case action
                        :done            finish-card
                        :x-card          x-card
                        :discard         discard-card
                        :pass            pass-card
                        :regen           regen-card
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
      (catch Exception e (println e)))
    (swap! world-atom update-in [:rooms room-id :game] execute)))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
