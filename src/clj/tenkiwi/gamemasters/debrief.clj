(ns tenkiwi.gamemasters.debrief
  "This is the game logic for debrief game"
  (:require [tenkiwi.tables.debrief :as tables]
            [tenkiwi.util :as util :refer [inspect]]))

(def valid-active-actions #{:regen :pass :discard :done :x-card :end-game :leave-game})
(def valid-inactive-actions #{:x-card :undo :leave-game :upvote-player :downvote-player})

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

(defn extract-vars [{:keys [active-player player-order company]
                     :as   game}]
  (let [next-player (:id (next-player player-order active-player))
        prev-player (:id (previous-player player-order active-player))
        values      (:values company)]
    {:leader-name  (get-in game [:dossiers :leader :agent-codename] "")
     :player-left  (get-in game [:dossiers prev-player :agent-codename] "")
     :player-right (get-in game [:dossiers next-player :agent-codename] "")
     :value-0      (nth values 0)
     :value-1      (nth values 1)
     :value-2      (nth values 2)}))

(defn replace-vars [game str-or-card]
  (let [text      (if (string? str-or-card)
                    str-or-card
                    (:text str-or-card))
        game-vars (extract-vars game)
        replaced  (clojure.string/replace (or text "")
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
     :type  :dossier
     :text   (str "Introduce your agent. Tell us their name, codename, team contribution and a fun fact about them.")
     :inputs [{:name      "agent-name"
               :label     "Agent Name"
               :value     random-name
               :generator :name
               }
              {:name      "agent-codename"
               :label     "Agent Codename"
               :value     random-codename
               :generator :codename
               }
              {:name      "agent-role"
               :label     "Team Role"
               :value     random-skill
               :generator :skill}]}))

(def round-texts
  [
   "**On the Case**\n\nPlease respond to the following prompts regarding this phase of the mission, paying special attention to actions exemplifying or counter to our company value {value-0}"
   "**Getting In**\n\nPlease respond to the following prompts regarding this phase of the mission, paying special attention to actions exemplifying or counter to our company value {value-1}"
   "**Getting Out**\n\nPlease respond to the following prompts regarding this phase of the mission, paying special attention to actions exemplifying or counter to our company value {value-2}"
   ])

(defn stage-card [round]
  {:id :act-transition
   :type :transition
   :text (nth round-texts round)})

(defn best-voting-round-card [round]
  {:id    (str "upvoting-" round)
   :round round
   :type :upvoting
   :text  (-> "Which agent best exemplified the company value of VALUE during the mission?"
              (clojure.string/replace #"VALUE" (str "{value-" round "}")))})

(defn worst-voting-round-card [round]
  {:id    (str "downvoting-" round)
   :round round
   :type :downvoting
   :text  (-> "Which agent least exemplified the company value of VALUE during the mission?"
              (clojure.string/replace #"VALUE" (str "{value-" round "}")))})

(def missions [
               ])

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
         round                 (:round card)
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
                                                                      :round round}) all-players)
                       :downvoting (mapv (partial player-button game {:rank :worst
                                                                      :round round}) all-players)
                       :dossier    [regen-action done-action]
                       [done-action pass])}))
  ([card active-player next-player]
   (build-active-card {} card active-player next-player)))

(defn build-inactive-version [game active-display]
  (let []
    active-display))

(defn build-inactive-card [active-player extra-text]
  (let [waiting (waiting-for active-player)
        waiting (if extra-text
                        (update waiting
                               :text
                               (partial str extra-text "\n\n"))
                        waiting)]

    {:card          waiting
     :extra-actions [leave-game-action]}))

(defn- build-starting-scores [{:keys [npc? id]} players]
  (let [ids (remove #(= id %) (map :id players))]
    (zipmap ids (cycle [5]))))

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
         :as         decks} (util/gather-decks "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=1113383423&single=true&output=tsv")
        question-decks      (group-by :stage questions)

        all-players (concat (into [] players)
                            npcs)
        card-count  (+ 21 (rand 10))
        company     {:name   "VISA"
                     :values (take 3 (shuffle tables/company-values))}
        new-game    {:player-order     (into [] players)
                     :player-scores    (into {}
                                             (map #(vector (:id %)
                                                           (build-starting-scores % players)) all-players))
                     :player-ranks     (zipmap
                                        (map :id players)
                                        (cycle [(zipmap [0 1 2]
                                                        (cycle [{:best nil :worst nil}]))]))
                     :all-players      all-players
                     :game-type        :debrief
                     :stage            :intro
                     :dossiers         dossiers
                     :discard          []
                     :company          company
                     :deck             (into []
                                             (concat (rest intro-cards)
                                                     [(company-values-card company)]
                                                     (map dossier-card players)
                                                     [(stage-card 0)]
                                                     (take card-count (shuffle (question-decks "0")))
                                                     [(best-voting-round-card 0) (worst-voting-round-card 0)]
                                                     [(stage-card 1)]
                                                     (take card-count (shuffle (question-decks "1")))
                                                     [(best-voting-round-card 1) (worst-voting-round-card 1)]
                                                     [(stage-card 2)]
                                                     (take card-count (shuffle (question-decks "2")))
                                                     [(best-voting-round-card 2) (worst-voting-round-card 2)]
                                                     [{:type :end
                                                       :text "TODO - finish"}]))
                     :active-player    (first players)
                     :active-display   (build-active-card (first intro-cards) first-player next-player)
                     :inactive-display (build-inactive-card first-player (:text (first intro-cards)))}]
    (doto world-atom
      (swap! update-in [:rooms room-id] assoc :game new-game))))

(defn extract-dossier [{:keys [inputs]}]
  (zipmap (map keyword (map :name inputs))
          (map :value inputs)))

(defn finish-card [game]
  (let [{:keys [player-order
                active-player
                dossiers
                discard
                deck
                stage]} game
        active-card     (get-in game [:active-display :card])
        dossiers        (if (#{:player-dossier} (:id active-card))
                          (assoc dossiers (:id active-player)
                                 (extract-dossier active-card))
                          dossiers)
        next-up         (next-player player-order active-player)
        discard         (cons active-card discard)
        next-card       (first deck)
        deck            (into [] (rest deck))
        next-stage      (:type next-card)
        next-next       (next-player player-order next-up)

        next-game          (assoc game
                               :deck deck
                               :stage next-stage
                               :dossiers dossiers
                               :discard discard
                               :active-player next-up)
        new-active-display (build-active-card next-game next-card next-up next-next)]
    (-> next-game
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
        next-stage      (:type next-card)

        next-game          (-> game
                            (assoc-in [:inactive-display :x-card-active?] false)
                            (assoc :deck deck
                                   :stage next-stage
                                   :discard discard))
        new-active-display (build-active-card next-game next-card active-player next-up)]
    (assoc next-game
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
           :active-display new-active-display)))

(defn push-uniq [coll item]
  (if (some #(= % item) coll)
    coll
    (into [item] coll)))

(defn rank-player
  [voter-id
   {:keys [id rank round]}
   {:keys [player-ranks] :as game}]
  (let [votes-remaining? (fn [x]
                           (let [ranks (get x :player-ranks)]
                            (some nil? (map #(get-in ranks [% round rank])
                                            (keys ranks)))))
        maybe-finish     #(if (votes-remaining? %)
                            %
                            (finish-card %))]
    (if (and (not= id voter-id)
            (nil? (get-in player-ranks [voter-id round rank])))
     (-> game
         (assoc-in [:player-ranks voter-id round rank] id)
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

(defn take-action [world-atom {:keys [uid room-id action params]}]
  (let [{:keys [player-order
                active-player
                active-display
                stage]
         :as   game} (get-in @world-atom [:rooms room-id :game])

        current-card   (:card active-display)
        active-player? (= (:id active-player) uid)
        valid?         (valid-action? active-player? action)
        do-next-state  (case action
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
                         :end-game        end-game)]
    (try
      (do-next-state game)
      (catch Exception e (println e)))
    (swap! world-atom update-in [:rooms room-id :game] do-next-state)))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
