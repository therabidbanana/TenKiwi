(ns tenkiwi.gamemasters.debrief
  "This is the game logic for debrief game"
  (:require [tenkiwi.tables.debrief :as tables]))

(def valid-active-actions #{:regen :pass :discard :done :x-card :end-game :leave-game})
(def valid-inactive-actions #{:x-card :undo :leave-game :upvote-player :downvote-player})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))

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

(def intro [
            "It is your turn, read the following. Each player will take turns reading cards aloud, and then hitting **\"Finish Turn\"**."
            "_You are all members of an elite organization known as VISA (Very Interesting Spy Agency)._"
            "_You were tasked with a mission of utmost importance, and have acheived your primary objective._"
            "_This acheivement did not come without cost - your team leader is now dead, and several secondary objectives were missed._"
            "_The **DUMBASS** System (Decisive Unilateral Mission Blame Assignment Software System) will now be used to evaluate your mission performance._"
            "_Please answer the prompts truthfully and to the best of your ability so that we may improve as a team for further missions._"
            ;; TODO: Handle mixed phases
            "When you have completed the introduction cards, take turns reading the prompts out loud. Interpret these questions and answer them, however you wish."
            "Other players may ask you questions or add clarifications on your turn, but how you respond is up to you."
            "The X option is available to all players at all times."
            "If you encounter a prompt, or an answer, that you don't want to be included in the game, use the 'X'. That content should be considered removed from the game."
            "If you draw a card that is removed this way, the border will be red. Simply \"Discard this\" to draw another card. You may 'X' a card you drew yourself."
            "You can also pass on your turn. To do so, use the pass button and say: \"I think you would be the best person to ask about this\""
            "A prompt can be passed around until someone applies the 'X' to it."
            "Continue answering, passing and X-ing questions until the end of each phase."
            "After each phase, agents will be asked to rank their peers on a key organizational value."
            "Throughout the game, you will be able to upvote or downvote a player for their Overall Contribution."
            "Before we begin, we will review the mission and reacquaint ourselves with the surviving agents."])

(defn company-values-card [{:keys [values]}]
  {:id :values
   :stage :intro
   :text (str "Always remember our organization's core values:\n\n* "
              (clojure.string/join "\n* " values))})

(defn dossier-card [{:keys []}]
  (let [random-name     (tables/random-name)
        random-codename (tables/random-codename)
        random-skill    (tables/random-skill)]
    {:id     :player-dossier
     :stage  :dossier
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

(def missions [
               ])

(def questions [
                "What was the one thing {leader} told you to pack for this mission?"
                "How do you know {player-left} failed to read the briefing?"
                "What insight did {player-right} have that proved most valuable during the mission?"
            ])

(def intro-cards (into []
                       (map-indexed #(hash-map :stage :intro :id %1 :text %2)
                                    intro)))

(def question-cards (into []
                          (map-indexed #(hash-map :stage :question :id %1 :text %2)
                                    questions)))


;; TODO: XSS danger?
(defn waiting-for
  [{:keys [user-name]}]
  {:id    "waiting"
   :stage :inactive
   :text  (str "It is " user-name "'s turn...")})

(defn next-player [player-order current-player]
  (let [curr-id    (:id current-player)
        curr-index (.indexOf (mapv :id player-order) curr-id)
        next-index (inc curr-index)
        next-index (if (>= next-index (count player-order))
                     0
                     next-index)]
    (nth player-order next-index)))

(defn build-active-card [card active-player next-player]
  (let [next-stage (or (:stage card) :intro)
        pass       {:action :pass
                    :text   (str "Pass card to " (:user-name next-player))}]
    {:card          card
     :extra-actions (case next-stage
                      :end      [leave-game-action]
                      :intro    [leave-game-action]
                      :dossier  [leave-game-action]
                      :question [leave-game-action])
     :actions       (case next-stage
                      :end      [pass end-game-action]
                      :intro    [done-action pass]
                      :dossier  [regen-action done-action]
                      :question [done-action pass])}))

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

        all-players (concat (into [] players)
                             npcs)
        card-count  (+ 21 (rand 10))
        company     {:name   "VISA"
                     :values (take 3 (shuffle tables/company-values))}
        new-game    {:player-order     (into [] players)
                     :player-scores    (into {}
                                             (map #(vector (:id %)
                                                           (build-starting-scores % players)) all-players))
                     :player-ranks     (zipmap [0 1 2]
                                               (cycle [{1 nil 2 nil 3 nil :unranked (map :id all-players)}]))
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
                                                     (take card-count (shuffle question-cards))
                                                     []))
                     :active-player    (first players)
                     :active-display   (build-active-card (first intro-cards) first-player next-player)
                     :inactive-display (build-inactive-card first-player (first intro))}]
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
        next-stage      (:stage next-card)
        next-next       (next-player player-order next-up)]
    (assoc game
           :deck deck
           :stage next-stage
           :dossiers dossiers
           :discard discard
           :active-player next-up
           :active-display (build-active-card next-card next-up next-next)
           :inactive-display (build-inactive-card next-up nil))))

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
        next-stage      (:stage next-card)]
    (-> game
        (assoc-in [:inactive-display :x-card-active?] false)
        (assoc :deck deck
               :stage next-stage
               :discard discard)
        (assoc
         :active-display (build-active-card next-card active-player next-up)))))


(defn pass-card [game]
  (let [{:keys [player-order
                active-player]} game
        active-card     (get-in game [:active-display :card])
        next-up         (next-player player-order active-player)
        next-next       (next-player player-order next-up)]
    (assoc game
           :active-player next-up
           :inactive-display (build-inactive-card next-up nil)
           :active-display (build-active-card active-card next-up next-next))))

(defn push-uniq [coll item]
  (if (some #(= % item) coll)
    coll
    (into [item] coll)))

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
        next-dossier (build-active-card (dossier-card active-player)
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
                         :tick-clock      tick-clock
                         ;; TODO allow players to leave game without ending
                         ;;; change action text
                         :leave-game      end-game
                         :end-game        end-game)]
    ;; (println next-state)
    (swap! world-atom update-in [:rooms room-id :game] do-next-state)))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
