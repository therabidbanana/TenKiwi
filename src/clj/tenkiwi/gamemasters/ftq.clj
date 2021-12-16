(ns tenkiwi.gamemasters.ftq
  "FTQ is a gamemaster supporting Descended by the Queen games"
  (:require [tenkiwi.util :as util :refer [inspect]]))

(def valid-active-actions #{:pass :discard :done :x-card :end-game :next-queen :previous-queen :leave-game})
(def valid-inactive-actions #{:x-card :undo :leave-game})

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

(def next-queen-action
  {:action :next-queen
   :class  :next-button
   :text   ">"})

(def previous-queen-action
  {:action :previous-queen
   :class  :previous-button
   :text   "<"})

(def discard-action
  {:action :discard
   :text   "[X] Discard this..."})

(def end-game-action
  {:action  :end-game
   :text    "End the Game"})

(defn normalize-twospace [text]
  (clojure.string/replace text #"\s\s" "\n\n"))

(defn build-normalized-card [type]
  (fn [id map]
    (-> (assoc map :id id :state type)
        (update :text normalize-twospace))))

(def intro-card (build-normalized-card :intro))
(def question-card (build-normalized-card :question))
(def end-card (build-normalized-card :end))
(def image-card (build-normalized-card :image))

(defn normalize-card [index {:keys [type] :as card}]
  (case type
    "intro" (intro-card index card)
    "question" (question-card index card)
    "image" (image-card index card)
    "end" (end-card index card)
    (question-card index card)))

(defn gather-decks [url]
  (let [cards (util/read-spreadsheet-data url normalize-card)]
    (group-by :state cards)))


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

(defn build-active-card [card active-player next-player]
  (let [next-state (or (:state card) :intro)
        pass       {:action :pass
                    :text   (str "Pass card to " (:user-name next-player))}]
    {:card              card
     :available-actions valid-active-actions
     :extra-actions     (case next-state
                          :end      [leave-game-action]
                          :intro    [next-queen-action previous-queen-action leave-game-action]
                          :question [leave-game-action])
     :actions           (case next-state
                          :end      [pass end-game-action]
                          :intro    [done-action pass]
                          :question [done-action pass])}))

(defn build-inactive-card [active-player extra-text]
  (let [waiting (waiting-for active-player)
        waiting (if extra-text
                  (update waiting
                          :text
                          (partial str extra-text "\n\n"))
                  waiting)]

    {:card              waiting
     :available-actions valid-inactive-actions
     :extra-actions     [leave-game-action]}))

(defn start-game [room-id
                  {:keys [game-url]
                   :or   {}}
                  {:keys [players]
                   :as game}]
  (let [decks        (gather-decks game-url)
        first-player (first players)
        next-player  (next-player players (:id first-player))
        card-count   (+ 21 (rand 10))
        new-game     {:player-order     (into [] players)
                      :game-type        :ftq
                      :state            :intro
                      :discard          []
                      :deck             (into []
                                              (concat (rest (:intro decks))
                                                      (take card-count (shuffle (:question decks)))
                                                      [(first (:end decks))]))
                      :active-player    (first players)
                      :queen-deck       (into [] (rest (:image decks)))
                      :queen            (first (:image decks))
                      :active-display   (build-active-card (first (:intro decks)) first-player next-player)
                      :inactive-display (build-inactive-card first-player (:text (first (:intro decks))))}]
    new-game))

(defn finish-card [game]
  (let [{:keys [player-order
                active-player
                discard
                deck
                state]} game
        active-card     (get-in game [:active-display :card])
        next-up         (next-player player-order active-player)
        discard         (cons active-card discard)
        next-card       (first deck)
        deck            (into [] (rest deck))
        next-state      (:state next-card)
        next-next       (next-player player-order next-up)]
    (assoc game
           :deck deck
           :state next-state
           :discard discard
           :active-player next-up
           :active-display (build-active-card next-card next-up next-next)
           :inactive-display (build-inactive-card next-up nil))))

(defn previous-queen [game]
  (let [{:keys [queen-deck
                queen]} game
        new-queen      (last queen-deck)
        new-queen-deck (into [queen] (pop queen-deck))]
    (assoc game
           :queen new-queen
           :queen-deck new-queen-deck)))

(defn next-queen [game]
  (let [{:keys [queen-deck
                queen]} game
        next-queen      (first queen-deck)
        next-queen-deck (conj (into [] (rest queen-deck)) queen)]
    (assoc game
           :queen next-queen
           :queen-deck next-queen-deck)))

(defn discard-card [game]
  (let [{:keys [player-order
                active-player
                discard
                deck
                state]} game
        active-card     (get-in game [:active-display :card])
        next-up         (next-player player-order active-player)
        discard         (cons active-card discard)
        next-card       (first deck)
        deck            (rest deck)
        next-state      (:state next-card)]
    (if next-card
      (-> game
         (assoc-in [:inactive-display :x-card-active?] false)
         (assoc :deck deck
                :state next-state
                :discard discard)
         (assoc
          :active-display (build-active-card next-card active-player next-up)))
      ;; Don't allow discard if deck empty
      game)))


(defn pass-card [game]
  (let [{:keys [player-order
                active-player]} game
        active-card     (inspect (get-in game [:active-display :card]))
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

(defn x-card [game]
  (let [{:keys []} game]
    (-> game
        (assoc-in [:active-display :x-card-active?] true)
        (update-in [:active-display :actions] push-uniq discard-action)
        (assoc-in [:inactive-display :x-card-active?] true))))

(defn end-game [game]
  nil)

(defn tick-clock [game]
  ;; No-op
  game)

(defn take-action [{:keys [uid room-id action]} {:keys [game]}]
  (let [{:keys [player-order
                active-player
                active-display
                state]
         :as   game}   game
        current-card   (:card active-display)
        active-player? (= (:id active-player) uid)
        valid?         (valid-action? active-player? action)
        do-next-state  (case action
                         :next-queen     next-queen
                         :previous-queen previous-queen
                         :done           finish-card
                         :x-card         x-card
                         :discard        discard-card
                         :pass           pass-card
                         :tick-clock     tick-clock
                         ;; TODO allow players to leave game without ending
                         ;;; change action text
                         :leave-game     end-game
                         :end-game       end-game)]
    ;; (println next-state)
    (do-next-state game)))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
