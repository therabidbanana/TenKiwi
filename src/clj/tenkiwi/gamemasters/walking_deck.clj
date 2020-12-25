(ns tenkiwi.gamemasters.walking-deck
  "This game master runs a Walking Deck game"
  #_(:require ))

(def valid-active-actions #{:discard :done :x-card :end-game :leave-game})
(def valid-inactive-actions #{:x-card :leave-game})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))

(def done-action
  {:action :done
   :text   "Finish Turn"})

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


;; TODO: XSS danger?
(defn waiting-for
  [{:keys [user-name]}]
  {:id    "waiting"
   :state :inactive
   :text  (str "It is " user-name "'s turn...")})

(defn next-player [player-order current-player]
  (let [curr-id    (:id current-player)
        curr-index (.indexOf (mapv :id player-order) curr-id)
        next-index (inc curr-index)
        next-index (if (>= next-index (count player-order))
                     0
                     next-index)]
    (nth player-order next-index)))

(defn build-active-card [{:keys [players-by-rank
                                 act
                                 active-player]
                          :as game-state}
                         card]
  (let [new-card {:state :fun
                  :color :red
                  :text  (str "You drew a " card)}]
    {:card          new-card
     :extra-actions [leave-game-action]
     :actions       (case act
                      4 [end-game-action]
                      0 [done-action]
                      1 [done-action]
                      2 [done-action]
                      3 [done-action])}))

(defn build-inactive-card [{:keys [players-by-rank
                                   act
                                   active-player]
                            :as game-state}
                           extra-text]
  (let [waiting (waiting-for active-player)
        waiting (if extra-text
                        (update waiting
                               :text
                               (partial str extra-text "\n\n"))
                        waiting)]

    {:card          waiting
     :extra-actions [leave-game-action]}))

(def card-suits #{:club :heart :spade :diamond})

(def card-ranks #{:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king})

(def playing-cards (for [rank card-ranks
                         suit card-suits]
                     (hash-map :rank rank :suit suit)))

(defn start-game [world-atom room-id]
  (let [players      (get-in @world-atom [:rooms room-id :players])
        first-player (first players)
        next-players (rest players)
        deck         (shuffle playing-cards)
        new-game     {:players-by-id    (zipmap (map :id players) players)
                      :players-by-rank  {}
                      :game-type        :walking-deck
                      :act              0
                      :discard          []
                      :deck             (rest deck)
                      :active-player    (first players)
                      :next-players     (rest players)}
        new-game     (assoc new-game
                      :active-display   (build-active-card new-game (first deck))
                      :inactive-display (build-inactive-card new-game "yo"))]
    (doto world-atom
      (swap! update-in [:rooms room-id] assoc :game new-game))))

(defn finish-card [game]
  (let [{:keys [player-order
                active-player
                next-players
                discard
                deck
                act]} game
        active-card     (get-in game [:active-display :card])
        next-players    (conj (into [] next-players) active-player)
        next-up         (first next-players)
        ;; This lets us push first player back in the mix (only single player)
        next-players    (rest next-players)
        discard         (cons active-card discard)
        next-card       (first deck)
        deck            (into [] (rest deck))
        next-state      0
        next-game       (assoc game
                               :deck deck
                               :next-players next-players
                               :discard discard
                               :active-player next-up
                               )]
    (assoc next-game
           :active-display (build-active-card next-game next-card)
           :inactive-display (build-inactive-card next-game nil))))

(defn discard-card [game]
  (let [{:keys [player-order
                active-player
                next-players
                discard
                deck
                state]} game
        active-card     (get-in game [:active-display :card])
        next-up         (first next-players)
        next-players    (conj (into [] (rest next-players)) active-player)
        discard         (cons active-card discard)
        next-card       (first deck)
        deck            (rest deck)
        next-state      (:state next-card)
        next-game       (-> game
                            (assoc-in [:inactive-display :x-card-active?] false)
                            (assoc :deck deck
                                   :next-players next-players
                                   :discard discard))]
    (-> next-game
        (assoc
         :active-display (build-active-card next-game next-card)))))


(defn push-uniq [coll item]
  (if (some #(= % item) coll)
    coll
    (into [item] coll)))

(defn x-card [game]
  (let [{:keys []} game]
    (-> game
        (assoc-in [:active-display :x-card-active?] true)
        (update-in [:active-display :actions] push-uniq discard-action)
        (assoc-in [:inactive-display :x-card-active?] true))))

(defn end-game [game]
  nil)

(defn take-action [world-atom {:keys [uid room-id action]}]
  (let [{:keys [player-order
                active-player
                active-display
                state]
         :as   game} (get-in @world-atom [:rooms room-id :game])

        current-card   (:card active-display)
        active-player? (= (:id active-player) uid)
        valid?         (valid-action? active-player? action)
        next-state     (case action
                         :done           (finish-card game)
                         :x-card         (x-card game)
                         :discard        (discard-card game)
                         ;; TODO allow players to leave game without ending
                         ;;; change action text
                         :leave-game     (end-game game)
                         :end-game       (end-game game))]
    ;; (println next-state)
    ;; TODO FIXME: Swapping on update after computing next state lets you do multiple turns
    ;; Need to compute next state atomically in the swap
    (swap! world-atom update-in [:rooms room-id] assoc :game next-state)))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
