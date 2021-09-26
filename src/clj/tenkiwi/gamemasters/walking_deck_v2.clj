(ns tenkiwi.gamemasters.walking-deck-v2
  "This game master runs a Walking Deck game"
  (:require [tenkiwi.util :as util :refer [inspect]]
            #_[walking-deck.common :as common]))

(def valid-active-actions #{:ready :pause-game :unpause-game :discard :done :x-card :end-game :choose-option :leave-game})
(def valid-inactive-actions #{:ready :pause-game :unpause-game :x-card :leave-game})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))

(def done-action
  {:action :done
   :text   "Finish Turn"})

(def ready-action
  {:action :ready
   :text   "Ready"})

(def leave-game-action
  {:action  :leave-game
   :confirm true
   :text    "End Game Now"})

(def pause-game-action
  {:action  :pause-game
   :text    "Pause Game"})

(def unpause-game-action
  {:action  :unpause-game
   :text    "Unpause Game"})

(def discard-action
  {:action :discard
   :text   "[X] Discard this..."})

(def pass-action
  {:action :done
   :text   "Stuck? Spend your turn freaking out instead."})

(def end-game-action
  {:action  :end-game
   :text    "End the Game"})

(def lose-game-action
  {:action  :end-game
   :text    "Everyone died! End Game"})

(def win-game-action
  {:action  :end-game
   :text    "Someone made it! End Game"})

;; TODO: XSS danger?
(defn waiting-for
  [{:keys [user-name]}]
  {:id    "waiting"
   :state :inactive
   :text  (str "It is " user-name "'s turn...")})


;;; ----------------------------------------
;; Utilities for building up the new decks
;;; ----------------------------------------

(def card-suits #{:clubs :hearts :spades :diamonds})

(def card-ranks #{:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king})

(def playing-cards (for [rank card-ranks
                         suit card-suits]
                     (hash-map :rank rank :suit suit)))

(defn- normalize-rank [string]
  (let [to-rank (merge {"k" :king
                        "a" :ace
                        "q" :queen
                        "j" :jack}
                       (zipmap (map #(cond (int? %)     (str %)
                                           (keyword? %) (name %))
                                    card-ranks)
                               card-ranks))
        input   (cond
                  (keyword? string) (name string)
                  (nil? string) "blank"
                  :else (clojure.string/lower-case string))]
    (get to-rank input (keyword input))))

(defn- normalize-suit [string]
  (let [to-suit
        (zipmap (map #(cond (keyword? %) (name %))
                     card-suits)
                card-suits)
        input   (cond
                  (keyword? string) (name string)
                  (nil? string) "blank"
                  :else (clojure.string/lower-case string))]
    (get to-suit input (keyword input))))

(defn- normalize-card-info [row-num map]
  (-> (util/normalize-card row-num map)
      (update :rank normalize-rank)
      (update :suit normalize-suit)))

(defn- require-type [rows]
  (->> rows (filter :type)))

;; TODO: Work on new style grouping
(defn gather-decks [maybe-str]
  (group-by :type (util/read-spreadsheet-data maybe-str
                                              normalize-card-info
                                              require-type)))

;;; ----------------------------------------

(defn- lookup-card
  "Find a card in a lookup map"
  ([lookup-map {:keys [rank suit]}]
   (get lookup-map [suit rank]))
  ([lookup-map card card-key]
   (-> (lookup-card lookup-map card)
       (get card-key))))

(defn- card-name
  ([{:keys [rank suit]}]
   (card-name rank suit))
  ([rank suit]
   (let [rank (cond (keyword? rank) (clojure.string/capitalize (name rank))
                    :else rank)
         suit (clojure.string/capitalize (name suit))]
     (str "the " rank " of " suit))))

(defn interpret-draw
  [{:keys [active-player next-players prompts]}
   {:keys [type rank suit] :as card}]
  (let [all-players                   (cons active-player next-players)
        {:keys            [dead? id
                           user-name]
         active-character :character} active-player

        starter?         (= :starter type)]
    (cond
      starter?
      (str "You are all {location} when {horde} strike!\n\n"
           "_When all players hit **\"Ready\"** the clocks will start and the game will begin._"
           )
      :else
      (str "Something went wrong - card text not found"))))

(defn replace-vars [{:keys [active-player
                            option-1 option-2
                            horde location]} str]
  (let [character (get-in active-player [:character :title])]
    (-> str
        (clojure.string/replace #"\{horde\}" (or horde "zombies"))
        (clojure.string/replace #"\{character\}" (or character "CHARACTER"))
        (clojure.string/replace #"\{location\}" (or location "LOCATION"))
        (clojure.string/replace #"\{option-1\}" (or option-1 "???"))
        (clojure.string/replace #"\{option-2\}" (or option-2 "???"))
       )))

(defn- build-card-prompts [type option-1 option-2]
  (let [option-1-label (type {:alive "Say something about..."
                              :dead "Establish why..."})
        option-2-label (type {:alive "We encounter..."
                              :dead "The horde..."})]
    [
     {:name :option-1 :selected? false :label option-1-label :description option-1}
     {:name :option-2 :selected? false :label option-2-label :description option-2}
     ]))

(defn build-active-card [{:keys [act
                                 paused?
                                 active-player
                                 active-display
                                 next-players
                                 horde
                                 location]
                          :as   game-state}
                         {:keys [text type option-1 option-2]
                          :as   card}]
  (let [all-players         (cons active-player next-players)
        survivors           (remove :dead? all-players)
        all-dead?           (empty? survivors)
        active-player-dead? (:dead? active-player)
        new-card            (assoc card
                                   :type (or type :unknown)
                                   :text (replace-vars
                                          (merge game-state card)
                                          (or text (interpret-draw game-state card))))
        new-card            (if (#{:alive :dead} type)
                              (assoc new-card
                                     :prompt-options (build-card-prompts type option-1 option-2))
                              new-card)
        actions             (cond
                              all-dead?                           [lose-game-action]
                              (and (> act 3) active-player-dead?) [lose-game-action]
                              (> act 3)                           [win-game-action]
                              (#{:alive :dead} (:type new-card))  [pass-action done-action]
                              (#{:starter} (:type new-card))      [ready-action]
                              :else                               [done-action]
                              )
        extra-actions       [(if paused? unpause-game-action
                                 pause-game-action)
                             leave-game-action]

        {:keys            [dead? id
                           user-name]
         active-character :character} active-player
        whos-up          (str
                          (:title active-character) " ("
                          user-name
                          ")'s turn...")]
    (if (#{:win? :lose? :death} type)
      (-> active-display
          (assoc :type type)
          (update :additional-prompts concat [(:text new-card)])
          (assoc :turn-marker whos-up)
          (assoc :extra-actions extra-actions)
          (assoc :actions actions))
      {:card          new-card
       :turn-marker   whos-up
       :available-actions #{:x-card :done :pass :pause :unpause :end-game}
       :extra-actions extra-actions
       :actions       actions})))

(defn build-inactive-card [{:keys [act
                                   paused?
                                   active-player]
                            :as   game-state}
                           extra-text]
  (let [waiting             (waiting-for active-player)
        {:keys [user-name]} active-player
        waiting             (if extra-text
                              (update waiting
                                      :text
                                      (partial str extra-text "\n\n"))
                              waiting)]

    {:card          waiting
     :actions       [(assoc done-action :disabled? true :text (str user-name "'s turn..."))]
     :available-actions #{:x-card :pause :unpause :end-game}
     :extra-actions [(if paused? unpause-game-action
                         pause-game-action)
                     leave-game-action]}))

;; (def introduction ["The Walking Deck is a story game played by reading and responding to prompts.\n\nEach turn, a player will read these prompts and when they have used them to add details to the story, press \"**Finish Turn**\""
;;                    "The clocks are ticking, and when they run out, whoever is speaking will be gruesomely killed!\n\nOnce dead, your prompts will change so you describe the forces against the survivors. If you die again, you'll simply add to the description like this once more."
;;                    "Each player will now introduce themselves as a character about to fight for their survival from **{horde}**."])

;; (def intro-cards (mapv #(hash-map :text % :type :intro) introduction))

(def starter-card
  {:type :starter
   :rank "x"
   :suit "x"})

(defn- parse-character-name [str]
  (let [[name & description] (clojure.string/split str #", " 2)]
    {:title       name
     :description (clojure.string/join ", " description)}))

(defn character-card [player [{option-1 :text} {option-2 :text}]]
  (let [character-1 (parse-character-name option-1)
        character-2 (parse-character-name option-2)]
    {:type      :character-intro
     :text      (str "Choose a character. How did you get here? What's your name? \n\n"
                     #_(clojure.string/join "\n\nor...\n\n" [option-1 option-2]))
     :prompt-options [{:character character-1
                       :name :option-1
                       :label (:title character-1)
                       :description (:description character-1)
                       :text option-1
                       :selected? false}
                      {:character character-2
                       :name :option-2
                       :label (:title character-2)
                       :description (:description character-2)
                       :text option-2
                       :selected? false}]
     :player-id (:id player)
     :player    player
     :character-1 character-1
     :character-2 character-2}))

(defn shuffle-discard [discard type]
  (->> discard
       (filter #(= type (:type %)))
       shuffle
       (into [])))

(defn act-timer! [room-id length]
  (if (= room-id "fast")
    (* length 6)
    (* length 60)))

(defn drama-timer! [room-id player-count]
  (let [ticks (if (= room-id "fast")
                6
                60)]
    (cond
      (< 5 player-count) (* 5 ticks)
      (< 3 player-count) (* 6 ticks)
      (< 1 player-count) (* 8 ticks)
      ;; This shouldn't be possible unless a person is playing alone
      :else (* 8 ticks))
    ))

;; (defn prepare-deck
;;   ([player-count prompts]
;;    (loop [deck (into [] (shuffle (map (fn [[suit rank]] (hash-map :rank rank :suit suit))
;;                                       (keys prompts))))
;;           chars []]
;;      (if (= (count chars) player-count)
;;        [chars (map #(assoc % :type :prompt) deck)]
;;        (let [next-char (first deck)
;;              deck      (into [] (rest deck))]
;;          ;; Careful - infinite loop coming if bug
;;          (if (and ((set (map :rank chars)) (:rank next-char))
;;                   (> 12 (count chars)))
;;            (recur (conj deck next-char) chars)
;;            (recur deck (conj chars next-char))))))))

(defn lookup-character
  ([card prompts]
   (let [{:keys [character]
          :as   card}           (lookup-card prompts card)
         [name & description] (clojure.string/split character #", " 2)]
     {:title       name
      :description (clojure.string/join ", " description)})))

(defn inactive-version [{:keys [active-player] :as game}
                        {:keys [card] :as active-display}]
  (let [{:keys [type]}      card
        {:keys [user-name]} active-player
        inactive-actions    [(assoc done-action :disabled? true :text (str user-name "'s turn..."))]]
    (case type
      :starter active-display
      :intro   (assoc active-display :actions inactive-actions)
      (assoc active-display :actions inactive-actions))))

(defn- group-by-uniq [key array]
  (reduce #(assoc %1 (get %2 key) %2) {} array))

(defn start-game [room-id params {:keys [players]}]
  (let [extra-players     (get params :extra-players 0)
        horde             (get params :horde "Zombies")
        location          (get params :location "in a mall")
        act-length        (get params :act-length 9)
        prompts           (gather-decks (get params :game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQBY3mq94cg_k3onDKmA1fa_L3AGbKVBfdxxeP04l73QVIXMkD4gEdG-e2ciex2jjTJjaKkdU1Vtaf1/pub?gid=963518572&single=true&output=tsv"))
        alive             (prompts :alive)
        dead              (prompts :dead)
        intro-cards       (->> (prompts :intro) (group-by :rank))
        gens              (->> (prompts :generator)
                               (group-by :rank))
        sounds            (->> (prompts :soundboard)
                               (group-by-uniq :rank))
        act-prompts       (->> (prompts :act-prompt)
                               (group-by-uniq :rank))
        original-players  players
        players           (take (+ extra-players (count original-players))
                                (cycle original-players))
        first-player      (first players)
        next-players      (rest players)
        player-count      (count players)
        players           (map-indexed #(assoc %2 :order %1) players)
        ;; [characters deck] (prepare-deck player-count prompts)

        ;; character-info    (map #(merge % (lookup-character % prompts)) characters)
        character-pairs    (->> (shuffle (:character gens))
                                cycle
                                (partition-all 2)
                                (take player-count))
        ;; Create dynamic cards with options for player to make
        character-cards   (map character-card players character-pairs)
        intro-deck        (concat (get intro-cards :first)
                                  character-cards
                                  (get intro-cards :last)
                                  [starter-card])

        new-game    {:game-type     :walking-deck-v2
                     :room-id       room-id
                     :horde         horde
                     :location      location
                     ;; :prompts       prompts
                     :prompts       {}
                     :generators    gens
                     :player-order  original-players
                     :act           0
                     :act-prompts   act-prompts
                     :act-prompt    (act-prompts 0)
                     :act-timer     (act-timer! room-id act-length)
                     :act-length    act-length
                     :drama-timer   (drama-timer! room-id player-count)
                     :discard       [(first intro-deck)]
                     :ready-players {}
                     :decks         {:intro (rest intro-deck)
                                     :dead (into [] dead)
                                     :alive (into [] alive)}
                     :active-player (first players)
                     :next-players  (rest players)}
        active-card (build-active-card new-game (first intro-deck))]
    (assoc new-game
           :active-display active-card
           :inactive-display (inactive-version new-game active-card))))

(def death-card {:type :death
                 :text "A fatal encounter occurs. **{character} is now dead.** Describe how {character} dies: "})

(def timer-card {:type :death
                 :text "The timer goes off and {character} is already dead! Describe the escalating struggles for the remaining characters."})

(def end-game-card {:type :win?
                    :text "After a final climatic situation, any surviving characters make it out alive.\n\nAs a group, feel free to describe their fates or leave it uncertain."})

(def dead-end-game-card {:type :lose?
                    :text "After a final climatic situation, nobody is left standing.\n\nAs a group, describe the last stand of the characters as they die."})

(def all-dead-card {:type :lose?
                    :text "Everyone has died."})

(defn draw-next [discard decks player]
  "Given discard and deck, draws next card, shuffling if needed"
  (let [deck-type (cond
                    (> (count (:intro decks)) 0)
                    :intro
                    (:dead? player)
                    :dead
                    :else
                    :alive)
        deck (get decks deck-type)]
    (let [next-card (first deck)
          [discard new-deck] (if (and (not= :intro deck-type) (empty? (rest deck)))
                               (do
                                 (println "Shuffling...")
                                 (let [[discard new-deck] (shuffle-discard deck-type)]
                                   [(cons next-card discard) (into [] new-deck)]))
                               [(cons next-card discard) (into [] (rest deck))])]
      [discard (assoc decks deck-type new-deck)])))

(defn update-player-if-selected! [display player]
  (let [options         (get-in display [:card :prompt-options] [])
        selected-option (first (filter :selected? options))
        character       (get selected-option :character)]
    (if character
      (assoc player :character character)
      player)))

(defn finish-card [{:keys [player-order
                           active-player
                           active-display
                           next-players
                           discard
                           decks
                           act-prompts
                           act]
                    :as   game}]
  (let [active-player      (update-player-if-selected! active-display active-player)
        all-players        (conj (into [] next-players) active-player)
        next-up            (first all-players)
        [discard decks]    (draw-next discard decks next-up)
        top-card           (first discard)
        ;; This lets us push first player back in the mix (only single player)
        next-players       (rest all-players)
        survivors          (remove :dead? all-players)
        all-dead?          (empty? survivors)
        next-player-alive? (:dead? next-up)
        next-card          (cond
                             all-dead?                          all-dead-card
                             (and (> act 3) next-player-alive?) end-game-card
                             (> act 3)                          dead-end-game-card
                             :else                              top-card)
        next-act           (if (and (= act 0)
                                    (#{:alive :dead} (:type next-card)))
                             (inc act)
                             act)
        next-act-prompt    (act-prompts next-act)
        next-game          (assoc game
                                  :next-players next-players
                                  :active-player next-up)
        active-card        (build-active-card next-game next-card)]
    (assoc next-game
           :decks decks
           :discard discard
           :act-prompt next-act-prompt
           :act next-act
           :active-display active-card
           :inactive-display (inactive-version next-game active-card))))

(defn discard-card [{:keys [player-order
                            active-player
                            active-display
                            next-players
                            prompts
                            discard
                            decks
                            state]
                     :as   game}]
  (let [[discard decks]       (draw-next discard decks active-player)
        top-card              (first discard)
        {:keys [how-you-die]} (lookup-card prompts top-card)
        specific-death        (-> death-card
                                  (update :text str " _" how-you-die "_."))

        next-card   (if (#{:death} (get-in active-display [:type]))
                      specific-death
                      top-card)
        active-card (build-active-card game next-card)]
    (assoc game
           :decks decks
           :discard discard
           :active-display active-card
           :inactive-display (inactive-version game active-card))))

(defn push-uniq [coll item]
  (if (some #(= % item) coll)
    coll
    (into [item] coll)))

(defn x-card [game]
  (let [{:keys [active-display]} game
        {{:keys [type]} :card}   active-display

        game (-> game
                 (assoc-in [:active-display :x-card-active?] true)
                 (assoc-in [:inactive-display :x-card-active?] true))]
    (cond
      ;; Don't allow a discard of intros / win & lose
      (#{:intro :character-intro :starter :lose? :win?} type)
      game
      :else
      (update-in game [:active-display :actions] push-uniq discard-action))
    ))

(defn choose-option [option-name game]
  (let [{:keys [active-display]} game
        {{:keys [type]} :card}   active-display
        prompt-options (get-in active-display [:card :prompt-options])
        updater (fn [{:keys [name] :as opt}]
                  (assoc opt :selected? (= option-name name)))
        prompt-options (map updater prompt-options)]
    (-> game
        (assoc-in [:active-display :card :prompt-options] prompt-options)
        (assoc-in [:inactive-display :card :prompt-options] prompt-options))
    ))

(defn pause-game [{:keys [:active-display] :as game}]
  (let [game        (assoc game :paused? true)
        active-card (build-active-card game (:card active-display))]
    (-> game
        (assoc :active-display active-card)
        (assoc :inactive-display (inactive-version game active-card))
        )))

(defn unpause-game [{:keys [:active-display] :as game}]
  (let [game        (assoc game :paused? false)
        active-card (build-active-card game (:card active-display))]
    (-> game
        (assoc :active-display active-card)
        (assoc :inactive-display (inactive-version game active-card))
        )))

(defn make-ready [uid game]
  (let [{:keys [ready-players
                player-order]} game
        ready-players          (assoc ready-players uid true)
        ]
    (if (some #(nil? (ready-players %)) (map :id player-order))
      (assoc game :ready-players ready-players)
      (finish-card (assoc game :ready-players {})))))

(defn end-game [game]
  nil)

(defn show-timer-card!
  [{:keys [prompts act active-player discard decks generators]
    :as game}]
  (let [currently-dead? (:dead? active-player)
        [discard decks] (draw-next discard decks active-player)
        current-card    (first discard)

        how-you-die   (->> (get-in generators [:how-you-die])
                           shuffle
                           first
                           :text)
        specific-death (-> death-card
                           (update :text str " _" how-you-die "_."))

        [kill-active?
         new-screen] (cond
                       (and (> act 3) (not currently-dead?))
                       ;; If the current player is alive at end of act 3 (act = 4)
                       [false (build-active-card game end-game-card)]
                       (> act 3)
                       ;; If the current player is dead at end of act 3 (act = 4)
                       [false (build-active-card game dead-end-game-card)]
                       ;; If player already dead
                       currently-dead?
                       [false (build-active-card game timer-card)]
                       :else
                       [true (build-active-card game specific-death)])
        next-game    (assoc game
                            :decks decks
                            :discard discard
                            :active-display new-screen
                            :inactive-display (inactive-version game new-screen))]
    (if kill-active?
      (-> next-game
          (assoc-in [:active-player :dead?] true)
          ;; Play death sound
          (assoc :broadcasts [[:->sound/trigger! :stinger]
                              [:->toast/show! "The timer goes off and someone died"]]))
      (-> next-game
          ;; Play timer expired sound (same for now)
          (assoc :broadcasts [[:->sound/trigger! :stinger]
                              [:->toast/show! "The timer goes off"]])))))

(defn tick-clock [game]
  (let [{:keys [act
                act-timer
                act-length
                act-prompts
                drama-timer
                active-display
                active-player
                next-players
                paused?
                room-id]} game
        all-players       (cons active-player next-players)
        player-count      (count all-players)
        survivors         (remove :dead? all-players)
        new-act-timer     (if (>= 1 act-timer)
                                  (act-timer! room-id act-length)
                                  (dec act-timer))
        new-drama-timer   (if (>= 1 drama-timer)
                                  (drama-timer! room-id player-count)
                                  (dec drama-timer))
        new-act?          (>= 1 act-timer)
        potential-death?  #(if (or (>= 1 drama-timer) new-act?)
                                   (show-timer-card! %)
                                   %)
        all-dead?         (empty? survivors)
        next-act          (if (or new-act? (= act 0))
                                  (inc act)
                                  act)
        next-act-prompt   (act-prompts next-act)]
    ;; Debug ticks
    ;; (println "tick" drama-timer new-drama-timer "act" act)
    (cond
      (= act 0) game ;; Game not technically started yet
      (> act 3) game ;; or over
      paused?   game ;; or paused
      all-dead? game ;; or lost
      :else
      (-> game
          (assoc-in [:act-timer] new-act-timer)
          (assoc-in [:act] next-act)
          (assoc-in [:act-prompt] next-act-prompt)
          (assoc-in [:drama-timer] new-drama-timer)
          potential-death?))))

(defn validated-mutator [uid action state-mutator]
  (fn [{:keys [player-order
               active-player
               active-display
               state]
        :as   game}]
    (let [active-player? (= (:id active-player) uid)
          valid?         (if (= :tick-clock action)
                           (= uid :timekeeper)
                           (valid-action? active-player? action))
          ]
      (if valid?
        (state-mutator game)
        (do
          (println "Invalid action from uid" action uid)
          game
          )))))

(defn take-action [{:keys [uid room-id action params]} {:keys [game]}]
  (let [state-mutator  (case action
                         :done          finish-card
                         :x-card        x-card
                         :ready         (partial make-ready uid)
                         :discard       discard-card
                         :pause-game    pause-game
                         :unpause-game  unpause-game
                         :choose-option (partial choose-option params)
                         :tick-clock    tick-clock
                         ;; TODO allow players to leave game without ending
                         ;;; change action text
                         :leave-game    end-game
                         :end-game      end-game)]

    ((validated-mutator uid action state-mutator) game)))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1))
