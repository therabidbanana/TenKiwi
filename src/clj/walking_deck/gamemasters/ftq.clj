(ns walking-deck.gamemasters.ftq
  "The host is in charge of moving users back and forth to rooms"
  #_(:require [com.stuartsierra.component :as component]))

;; TODO: Extract these - duplicated in host?
(defn ->players [{:keys [chsk-send!]} uids message]
  (doseq [uid uids]
    (chsk-send! uid message)))

(defn ->player [system uid message]
  (->players system [uid] message))

(defn ->room
  [{:keys [register] :as system} room message]
  (if-let [players (-> register :world deref
                       (get-in [:rooms room :players]))]
    (->players system (map :id players) message)))


#_(defn set-player-room
  ([world-atom uid room-id]
   (let [user-info (get-in @world-atom [:player-info uid])]
     (set-player-room world-atom uid room-id user-info)))
  ([world-atom uid room-id user-info]
   (let [room (or (get-room world-atom room-id)
                  {:id room-id
                   :players []})
         _ (println room)
         new-room (update-in room [:players] conj user-info)]
     (doto world-atom
       (swap! update-in [:players] assoc uid room-id)
       (swap! update-in [:player-info] assoc uid user-info)
       (swap! update-in [:rooms] assoc room-id new-room)))))


(def intro [
            "Each player will take turns reading cards aloud, and then hitting done."
            "The land you live in has been at war for as long as any of you have been alive."
            "The Queen has decided to undertake a long and perilous journey to broker an alliance with a distant power."
            "The Queen has chosen all of you, and no one else, to be her retinue, and accompany her on this journey."
            "She chose you because she knows that you love her."
            "When you have completed the introduction cards, take turns reading the prompts out loud. Interpret these questions and answer them, however you wish."
            "Other players may ask you questions or make suggestions on your turn, but whether you answer those questions or take those suggestions is entirely up to you."
            "The X-Card option is available to all players at all times."
            "If you encounter a prompt, or an answer, that you don't want to be included in the game, use the X-Card. That content should be considered removed from the game."
            "If you draw a card that is removed this way, simply draw another card. You may 'X' a card you drew yourself."
            "You can also pass on your turn. To do so, use the pass button and say: \"I'd like to hear your answer to this question\""
            "A prompt can be passed around until someone applies the 'X' to it."
            "Continue answering, passing and X-ing questions until 'The Queen is under attack' card is drawn."
            "Each player should answer this question in turn. Then, the game is over."
            ;; TODO - Whoever wants to can draw the first prompt card.
            ])

(def questions [
                "The Queen is not your queen. Why do you serve her anyway?"
                "The Queen knows something about you that no one else does. What is it?"
                "What do you usually do for the royal family? Why does that make you an unlikely choice for this journey?"
                "What did you bring with you that endangers the Queen?"
                "You think of someone in this retinue as the Queen's favorite. Who? What makes you think this?"
                "What promise did the Queen make to you before this journey? Do you think she'll keep it?"
                "There is a false rumor about you and the Queen, back at the royal court. What is it? How did it start?"
                "You suspect this journey isn't about diplomatic negotiations. What else do you believe is going on, and why?"
                "What makes the Queen beautiful, in your eyes?"
                "You are considered beautiful by almost everyone you meet. How does the Queen make you question that perception?"
                "The Queen had you punished, once. What about the memory of that will stay with you forever?"
                "The Queen thinks more highly of you than you do of yourself.  How do you know this?"
                "You sometimes think you might be the Queen's favorite. Why? And why does this worry you?"
                "How does the Queen remind you of her status while on the journey?"
                "What brings out the Queen's cruelty?"
                "What makes the Queen ugly, in your eyes?"
                "When was the last time the Queen hurt you?"
                "What do you do for the Queen that anyone else can do, and why does she make you do it?"
                "There is a part of you that does not want peace in this land. Why are you attached to the war?"
                "There is someone else in this retinue that you love, besides the Queen. How and why are you keeping it a secret?"
                "Why do you think the Queen trusts you enough to bring you on this journey?"
                "When was the last time the Queen showed you real kindness?"
                "What part of this journey will be the most difficult? How do you pull the rest of the retinue through it?"
                "What brings out the Queen's kindness?"
                "What do you do for the Queen that no one else can do?"
                "What did you bring with you to protect the Queen?"
                "When did you know that you were in love with the Queen?"
                "The Queen sometimes shows an interest in your personal life. How do you respond to that? Why?"
                "You are considered ugly by almost everyone you meet. How does the Queen make you question that perception?"
                "You saw the Queen do something terrible to keep the retinue safe. What was it? Did you come to respect her more or less afterward?"
                "You have a personal connection to the land you are currently traveling through. What makes you want to stay, and why don't you?"
                "Who is this distant power you are traveling to, and why do they make you uneasy?"
                "Why are some others at the royal court jealous of your relationship with the Queen?"
                "What is something you do for the royal family that has prepared you well for this journey?"
                "You saved the Queen's life once--how?"
                "You arranged for the Queen to be ambushed on this journey. What did they offer you?"
                "When did you know you would never forgive the Queen, and why?"
                "The Queen lights a fire in you. What is it?"
                "You were summoned to a private meeting with the Queen, once. Why did you feel disappointed afterward?"
                "The Queen gave you a compliment, once. What was it, and why have her words stayed with you?"
                "What did you do to disappoint the Queen on this journey?"
                "The Queen trusts you, but no one else in the royal court does. Why?"
                "The Queen is responsible for the death of someone you loved. Who? What happened?"
                "The Queen touched you, once. What about the memory of that will stay with you forever?"
                "What do you do that pleases the Queen on this journey?"
            ])

(def intro-cards (into []
                       (map-indexed #(hash-map :state :intro :id %1 :text %2)
                                    intro)))

(def question-cards (into []
                          (map-indexed #(hash-map :state :question :id %1 :text %2)
                                    questions)))

(def queen-attacked {:id   "attacked"
                     :state :end
                     :text "The queen is under attack. Do you defend her?"})


;; TODO: XSS danger?
(defn waiting-for
  [{:keys [user-name]}]
  {:id    "waiting"
   :state :inactive
   :text  (str "It is " user-name "'s turn...")})

(defn next-player [player-order current-player]
  (let [curr-index (.indexOf (mapv :id player-order) current-player)
        next-index (inc curr-index)
        next-index (if (>= next-index (count player-order))
                     0
                     next-index)]
    (get player-order next-index)))

(defn start-game [world-atom room-id]
  (let [players      (get-in @world-atom [:rooms room-id :players])
        done            {:action :done
                         :text   "Finish Turn"}
        discard         {:action :discard
                         :text   "Discard this..."}
        pass            {:action :pass
                         :text   (str "Pass to " (:user-name (next-player players (:id (first players)))))}
        new-game     {:player-order     (into [] players)
                      :game             :ftq
                      :state            :intro
                      :discard          []
                      :deck             (into []
                                              (concat (rest intro-cards)
                                                      (take 20 (shuffle question-cards))
                                                      [queen-attacked]))
                      :active-player    (:id (first players))
                      :active-display   {:card (first intro-cards)
                                         :actions [done pass discard]}
                      :inactive-display {:card (waiting-for (first players))}}]
    (doto world-atom
      (swap! update-in [:rooms room-id] assoc :game new-game))))

(def valid-active-actions #{:pass :discard :done :x-card :end-game})
(def valid-inactive-actions #{:x-card :undo})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))


(defn finish-card [game]
  (let [{:keys [player-order
                active-player
                discard
                deck
                state]} game
        active-card     (get-in game [:active-display :card])
        next-player     (next-player player-order active-player)
        discard         (cons active-card discard)
        next-card       (first deck)
        deck            (rest deck)
        next-state      (:state next-card)
        next-next       (next-player player-order (:id next-player))
        ;; TODO: Why is this busted?
        _ (println player-order next-next)

        pass            {:action :pass
                         :text   (str "Pass to " (:user-name next-next))}
        done            {:action :done
                         :text   "Done"}
        discard         {:action :discard
                         :text   "Discard this..."}
        end-game        {:action :end-game
                         :text   "End the Game"}]
    (assoc game
           :deck deck
           :state next-state
           :discard discard
           :active-player (:id next-player)
           :active-display {:card    next-card
                            :actions (case next-state
                                       :end      [pass end-game]
                                       :intro    [done pass discard]
                                       :question [done pass discard])}
           :inactive-display {:card (waiting-for next-player)})))

(defn discard-card [game]
  (let [{:keys [player-order
                active-player
                discard
                deck
                state]} game
        active-card     (get-in game [:active-display :card])
        next-player     (next-player player-order active-player)
        discard         (cons active-card discard)
        next-card       (first deck)
        deck            (rest deck)
        next-state      (:state next-card)

        pass            {:action :pass
                         :text   (str "Pass to " (:user-name next-player))}
        done            {:action :done
                         :text   "Finish Turn"}
        discard         {:action :discard
                         :text   "Discard this..."}
        end-game        {:action :end-game
                         :text   "End the Game"}]
    (assoc game
           :deck deck
           :state next-state
           :discard discard
           :active-display {:card    next-card
                            :actions (case next-state
                                       :end      [pass end-game]
                                       :intro    [done pass discard]
                                       :question [done pass discard])})))


(defn pass-card [game]
  (let [{:keys [player-order
                active-player
                deck
                state]} game
        next-player     (next-player player-order active-player)]
    (assoc game
           :active-player (:id next-player))))

(defn end-game [game]
  nil)

(defn take-action [world-atom {:keys [uid room-id action]}]
  (let [{:keys [player-order
                active-player
                active-display
                state]
         :as   game} (get-in @world-atom [:rooms room-id :game])

        current-card   (:card active-display)
        active-player? (= active-player uid)
        valid?         (valid-action? active-player? action)
        next-state     (case action
                         :done     (finish-card game)
                         :discard  (discard-card game)
                         :pass     (pass-card game)
                         :end-game (end-game game))]
    (println next-state)
    (swap! world-atom update-in [:rooms room-id] assoc :game next-state)))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
