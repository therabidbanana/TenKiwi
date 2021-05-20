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

#_(def intro [
            "Each player will take turns reading cards aloud, and then hitting **\"Finish Turn\"**."
            "_The land you live in has been at war for as long as any of you have been alive._"
            "_The Queen has decided to undertake a long and perilous journey to broker an alliance with a distant power._"
            "_The Queen has chosen all of you, and no one else, to be her retinue, and accompany her on this journey._"
            "_She chose you because she knows that you love her._"
            "You are welcome to choose a queen below.\n\nIf there is one that seems right for the group, leave it to inspire your story."
            "When you have completed the introduction cards, take turns reading the prompts out loud. Interpret these questions and answer them, however you wish."
            "Other players may ask you questions or make suggestions on your turn, but whether you answer those questions or take those suggestions is entirely up to you."
            "The X option is available to all players at all times."
            "If you encounter a prompt, or an answer, that you don't want to be included in the game, use the 'X'. That content should be considered removed from the game."
            "If you draw a card that is removed this way, the border will be red. Simply draw another card. You may 'X' a card you drew yourself."
            "You can also pass on your turn. To do so, use the pass button and say: \"I'd like to hear your answer to this question\""
            "A prompt can be passed around until someone applies the 'X' to it."
            "Continue answering, passing and X-ing questions until 'The Queen is under attack' card is drawn."
            "Each player should answer this question in turn. Then, the game is over."
            ;; TODO - Whoever wants to can draw the first prompt card.
            ])

#_(def questions [
                "The Queen is not your queen.\n\nWhy do you serve her anyway?"
                "The Queen knows something about you that no one else does.\n\nWhat is it?"
                "What do you usually do for the royal family?\n\nWhy does that make you an unlikely choice for this journey?"
                "What did you bring with you that endangers the Queen?"
                "You think of someone in this retinue as the Queen's favorite.\n\nWho?\n\nWhat makes you think this?"
                "What promise did the Queen make to you before this journey?\n\nDo you think she'll keep it?"
                "There is a false rumor about you and the Queen, back at the royal court.\n\nWhat is it?\n\nHow did it start?"
                "You suspect this journey isn't about diplomatic negotiations.\n\nWhat else do you believe is going on, and why?"
                "What makes the Queen beautiful, in your eyes?"
                "You are considered beautiful by almost everyone you meet.\n\nHow does the Queen make you question that perception?"
                "The Queen had you punished, once.\n\nWhat about the memory of that will stay with you forever?"
                "The Queen thinks more highly of you than you do of yourself.\n\nHow do you know this?"
                "You sometimes think you might be the Queen's favorite.\n\nWhy?\n\nAnd why does this worry you?"
                "How does the Queen remind you of her status while on the journey?"
                "What brings out the Queen's cruelty?"
                "What makes the Queen ugly, in your eyes?"
                "When was the last time the Queen hurt you?"
                "What do you do for the Queen that anyone else can do, and why does she make you do it?"
                "There is a part of you that does not want peace in this land.\n\nWhy are you attached to the war?"
                "There is someone else in this retinue that you love, besides the Queen.\n\nHow and why are you keeping it a secret?"
                "Why do you think the Queen trusts you enough to bring you on this journey?"
                "When was the last time the Queen showed you real kindness?"
                "What part of this journey will be the most difficult?\n\nHow do you pull the rest of the retinue through it?"
                "What brings out the Queen's kindness?"
                "What do you do for the Queen that no one else can do?"
                "What did you bring with you to protect the Queen?"
                "When did you know that you were in love with the Queen?"
                "The Queen sometimes shows an interest in your personal life.\n\nHow do you respond to that?\n\nWhy?"
                "You are considered ugly by almost everyone you meet.\n\nHow does the Queen make you question that perception?"
                "You saw the Queen do something terrible to keep the retinue safe.\n\nWhat was it?\n\nDid you come to respect her more or less afterward?"
                "You have a personal connection to the land you are currently traveling through.\n\nWhat makes you want to stay, and why don't you?"
                "Who is this distant power you are traveling to, and why do they make you uneasy?"
                "Why are some others at the royal court jealous of your relationship with the Queen?"
                "What is something you do for the royal family that has prepared you well for this journey?"
                "You saved the Queen's life once—how?"
                "You arranged for the Queen to be ambushed on this journey.\n\nWhat did they offer you?"
                "When did you know you would never forgive the Queen, and why?"
                "The Queen lights a fire in you.\n\nWhat is it?"
                "You were summoned to a private meeting with the Queen, once.\n\nWhy did you feel disappointed afterward?"
                "The Queen gave you a compliment, once.\n\nWhat was it, and why have her words stayed with you?"
                "What did you do to disappoint the Queen on this journey?"
                "The Queen trusts you, but no one else in the royal court does.\n\nWhy?"
                "The Queen is responsible for the death of someone you loved.\n\nWho?\n\nWhat happened?"
                "The Queen touched you, once.\n\nWhat about the memory of that will stay with you forever?"
                "What do you do that pleases the Queen on this journey?"
            ])

#_(def queen-images [
                   "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse2.mm.bing.net%2Fth%3Fid%3DOIP.6JYz-wr2khmwViv4FgpujAHaFj%26pid%3DApi&f=1"
             "images/queens/1.jpg"
             "images/queens/2.jpg"
             "images/queens/3.jpg"
             "images/queens/4.jpg"
             "images/queens/5.jpg"
             "images/queens/6.jpg"
             "images/queens/7.jpg"
             ])

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

#_(def intro-cards (into []
                       (map-indexed #(hash-map :state :intro :id %1 :text %2)
                                    intro)))

#_(def question-cards (into []
                          (map-indexed #(hash-map :state :question :id %1 :text %2)
                                    questions)))

#_(def queen-attacked {:id   "attacked"
                     :state :end
                     :text "The queen is under attack.\n\nDo you defend her?"})

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
    {:card          card
     :extra-actions (case next-state
                      :end      [leave-game-action]
                      :intro    [next-queen-action previous-queen-action leave-game-action]
                      :question [leave-game-action])
     :actions       (case next-state
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

    {:card          waiting
     :extra-actions [leave-game-action]}))

(defn start-game [room-id
                  {:keys [game-url]
                   :or   {game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQy0erICrWZ7GE_pzno23qvseu20CqM1XzuIZkIWp6Bx_dX7JoDaMbWINNcqGtdxkPRiM8rEKvRAvNL/pub?gid=0&single=true&output=tsv"}}
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
