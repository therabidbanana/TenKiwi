(ns tenkiwi.gamemasters.walking-deck
  "This game master runs a Walking Deck game"
  #_(:require))

(def valid-active-actions #{:pause-game :unpause-game :discard :done :x-card :end-game :leave-game})
(def valid-inactive-actions #{:pause-game :unpause-game :x-card :leave-game})

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

(def pause-game-action
  {:action  :pause-game
   :text    "Pause Game"})

(def unpause-game-action
  {:action  :unpause-game
   :text    "Unpause Game"})

(def discard-action
  {:action :discard
   :text   "[X] Discard this..."})

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

(defn next-player [player-order current-player]
  (let [curr-id    (:id current-player)
        curr-index (.indexOf (mapv :id player-order) curr-id)
        next-index (inc curr-index)
        next-index (if (>= next-index (count player-order))
                     0
                     next-index)]
    (nth player-order next-index)))


(def card-suits #{:clubs :hearts :spades :diamonds})

(def card-ranks #{:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king})

(def playing-cards (for [rank card-ranks
                         suit card-suits]
                     (hash-map :rank rank :suit suit)))

(def act-prompts {1 "During this act, focus your story on the past that led us here."
                  2 "During this act, focus your story on plans for the future, to escape or defeat the horde."
                  3 "During this act, focus your story on the present and how things are getting worse."})

(defn lookup-card [lookup-map {:keys [rank suit]}]
  (get lookup-map [suit rank]))

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
                  (nil? string) :blank
                  :else (clojure.string/lower-case string))]
    (get to-rank input :unknown)))

(defn- normalize-suit [string]
  (let [to-suit
        (zipmap (map #(cond (keyword? %) (name %))
                     card-suits)
                card-suits)
        input   (cond
                  (keyword? string) (name string)
                  (nil? string) :blank
                  :else (clojure.string/lower-case string))]
    (get to-suit input :unknown)))

(defn- normalize-card-info [map]
  (-> map
      (update :rank normalize-rank)
      (update :suit normalize-suit)))

(defn- read-spreadsheet-data [url]
  (let [lines
        (->> (slurp url)
             (clojure.string/split-lines)
             (map #(clojure.string/split % #"\t")))
        header (first lines)
        rest   (rest lines)
        keys   (map keyword header)
        rows   (map #(zipmap keys %) rest)]
    (map normalize-card-info rows)))

(defn- to-lookup-map [card-rows]
  (into {}
        (for [{:keys [rank suit]
               :as row} card-rows]
          [[suit rank] row])))

(comment
  ;; Use eval and replace to pull in a published sheet with tsv
  ;; Use :s/, \[/,\n\]/g
  ;; (read-spreadsheet-data url)
  (to-lookup-map (read-spreadsheet-data "https://docs.google.com/spreadsheets/d/e/2PACX-1vQBY3mq94cg_k3onDKmA1fa_L3AGbKVBfdxxeP04l73QVIXMkD4gEdG-e2ciex2jjTJjaKkdU1Vtaf1/pub?gid=0&single=true&output=tsv")))

(def prompts
  {[:clubs :jack]     {:rank :jack, :suit :clubs, :reflect-on "If Someone Else Will Die", :encounter-something "Eternal Purgatory", :establish-something "We Can't Close It", :the-horde "Are a friend", :someone-else "They deliver violence and tragedy deliberately"},
   [:spades 9]        {:rank 9, :suit :spades, :reflect-on "Our Values", :encounter-something "Whispering", :establish-something "We Mustn't Reveal Ourselves", :the-horde "Are full of animals", :someone-else "They cause violence and tragedy by accident"},
   [:diamonds 2]      {:rank 2, :suit :diamonds, :reflect-on "What I Want", :encounter-something "Alarms", :establish-something "We Have A Gun", :the-horde "Get inside", :someone-else "They are saved or protected by someone else"},
   [:hearts :ace]     {:rank :ace, :suit :hearts, :reflect-on "Your Place in Society", :encounter-something "Smoke", :establish-something "We Need Transport", :the-horde "Are on fire", :someone-else "They put themselves at risk helping others"},
   [:spades 8]        {:rank 8, :suit :spades, :reflect-on "Depression", :encounter-something "A Loved One", :establish-something "We Mustn't Get Hurt", :the-horde "Show low cunning", :someone-else "They cause violence and tragedy by accident"},
   [:clubs :ace]      {:rank :ace, :suit :clubs, :reflect-on "You Being Betrayed", :encounter-something "Ashes", :establish-something "We Can't Find It", :the-horde "Are smelly", :someone-else "They deliver violence and tragedy deliberately"},
   [:spades 10]       {:rank 10, :suit :spades, :reflect-on "This is Without Meaning ", :encounter-something "Stone", :establish-something "We Mustn't Give In", :the-horde "Torture for fun", :someone-else "They cause violence and tragedy by accident"},
   [:hearts :queen]   {:rank :queen, :suit :hearts, :reflect-on "Who Is Coming to Save You", :encounter-something "Death", :establish-something "We Need Order", :the-horde "Appear innocent", :someone-else "They put themselves at risk helping others"},
   [:diamonds :queen] {:rank :queen, :suit :diamonds, :reflect-on "Who Isn't Coming to Save You", :encounter-something "Dying", :establish-something "We Have a Crowbar", :the-horde "Appear helpful", :someone-else "They are saved or protected by someone else"},
   [:diamonds :jack]  {:rank :jack, :suit :diamonds, :reflect-on "If You'll Die", :encounter-something "A Fresh Hell", :establish-something "We Have A Tactical Edge", :the-horde "Are an enemy", :someone-else "They are saved or protected by someone else"},
   [:clubs :king]     {:rank :king, :suit :clubs, :reflect-on "The Last Thing You'll Ever See", :encounter-something "Debasement", :establish-something "We Can't Control It", :the-horde "Ride the bus", :someone-else "They deliver violence and tragedy deliberately"},
   [:hearts 10]       {:rank 10, :suit :hearts, :reflect-on "This is Fair Punishment", :encounter-something "Guns", :establish-something "We Need A Way Out", :the-horde "Torture to betrayal", :someone-else "They put themselves at risk helping others"},
   [:spades 2]        {:rank 2, :suit :spades, :reflect-on "What I'm Doing", :encounter-something "Silence", :establish-something "We Mustn't Be Heard", :the-horde "Almost reach", :someone-else "They cause violence and tragedy by accident"},
   [:diamonds 9]      {:rank 9, :suit :diamonds, :reflect-on "Our Culture", :encounter-something "Yelling", :establish-something "We Have A Cellphone", :the-horde "Control animals", :someone-else "They are saved or protected by someone else"},
   [:hearts 7]        {:rank 7, :suit :hearts, :reflect-on "Where I Should Be", :encounter-something "Praising God", :establish-something "We Need Food", :the-horde "Overwhelm us", :someone-else "They put themselves at risk helping others"},
   [:clubs 10]        {:rank 10, :suit :clubs, :reflect-on "This is A Blessing in Disguise", :encounter-something "Steel", :establish-something "We Can't Open It", :the-horde "Torture for fear", :someone-else "They deliver violence and tragedy deliberately"},
   [:diamonds 8]      {:rank 8, :suit :diamonds, :reflect-on "Anger", :encounter-something "Someone", :establish-something "We Have a Contact", :the-horde "Form a strategy", :someone-else "They are saved or protected by someone else"},
   [:diamonds 7]      {:rank 7, :suit :diamonds, :reflect-on "Where I Could Be", :encounter-something "Fearing God", :establish-something "We Have a Promise", :the-horde "Press in on us", :someone-else "They are saved or protected by someone else"},
   [:clubs :queen]    {:rank :queen, :suit :clubs, :reflect-on "Who Might Come to Silence You", :encounter-something "Aging", :establish-something "We Can't Carry It", :the-horde "Appear harmless", :someone-else "They deliver violence and tragedy deliberately"},
   [:spades 6]        {:rank 6, :suit :spades, :reflect-on "Blame Ourselves", :encounter-something "Fawn", :establish-something "We Mustn't Miss Our Chance", :the-horde "Break the bone", :someone-else "They cause violence and tragedy by accident"},
   [:spades :king]    {:rank :king, :suit :spades, :reflect-on "Your Last Dying Wish", :encounter-something "Desecration", :establish-something "We Mustn't Be Too Late", :the-horde "Crash cars", :someone-else "They cause violence and tragedy by accident"},
   [:spades 5]        {:rank 5, :suit :spades, :reflect-on "Journalists", :encounter-something "Falling", :establish-something "We Mustn't Be Left Behind", :the-horde "Drool and vomit", :someone-else "They cause violence and tragedy by accident"},
   [:hearts 2]        {:rank 2, :suit :hearts, :reflect-on "Who I Am", :encounter-something "Sirens", :establish-something "We Need Bandages", :the-horde "Grab someone", :someone-else "They put themselves at risk helping others"},
   [:hearts 6]        {:rank 6, :suit :hearts, :reflect-on "Blame the Military", :encounter-something "Flight", :establish-something "We Need More Bullets", :the-horde "Suck the juices", :someone-else "They put themselves at risk helping others"},
   [:hearts :king]    {:rank :king, :suit :hearts, :reflect-on "The Last People Alive", :encounter-something "Defiance", :establish-something "We Need Teamwork", :the-horde "Down airplanes", :someone-else "They put themselves at risk helping others"},
   [:hearts 5]        {:rank 5, :suit :hearts, :reflect-on "Cops", :encounter-something "Shattering", :establish-something "We Need Information", :the-horde "Swarm like locusts", :someone-else "They put themselves at risk helping others"},
   [:hearts :jack]    {:rank :jack, :suit :hearts, :reflect-on "If You'll Live", :encounter-something "A Kind of Heaven", :establish-something "We Need A Leader", :the-horde "Are a relative", :someone-else "They put themselves at risk helping others"},
   [:clubs 4]         {:rank 4, :suit :clubs, :reflect-on "Where This Is Leading", :encounter-something "A Judgement", :establish-something "We Can't Get Out", :the-horde "Find weapons", :someone-else "They deliver violence and tragedy deliberately"},
   [:diamonds 5]      {:rank 5, :suit :diamonds, :reflect-on "Scientists", :encounter-something "Collapsing", :establish-something "We Have A Pile of Cash", :the-horde "Devour the still alive", :someone-else "They are saved or protected by someone else"},
   [:hearts 4]        {:rank 4, :suit :hearts, :reflect-on "Where They Came From", :encounter-something "A Corpse", :establish-something "We Need Drugs", :the-horde "Take control", :someone-else "They put themselves at risk helping others"},
   [:spades :ace]     {:rank :ace, :suit :spades, :reflect-on "Your Chance of Survival", :encounter-something "Fire", :establish-something "We Mustn't Follow Orders", :the-horde "Are everywhere", :someone-else "They cause violence and tragedy by accident"},
   [:hearts 9]        {:rank 9, :suit :hearts, :reflect-on "Our Country", :encounter-something "Screaming", :establish-something "We Need To Rest", :the-horde "Infest animals", :someone-else "They put themselves at risk helping others"},
   [:diamonds :king]  {:rank :king, :suit :diamonds, :reflect-on "The Last Place To Hide", :encounter-something "Deference", :establish-something "We Have a Bible", :the-horde "Derail the train", :someone-else "They are saved or protected by someone else"},
   [:clubs 3]         {:rank 3, :suit :clubs, :reflect-on "The Future", :encounter-something "Gorging", :establish-something "We Can't See", :the-horde "Get back up", :someone-else "They deliver violence and tragedy deliberately"},
   [:spades 7]        {:rank 7, :suit :spades, :reflect-on "Where I Will Never Be", :encounter-something "Killing God", :establish-something "We Mustn't Believe Them", :the-horde "Come from behind", :someone-else "They cause violence and tragedy by accident"},
   [:spades :queen]   {:rank :queen, :suit :spades, :reflect-on "Who Has Abandoned You", :encounter-something "Rebirth", :establish-something "We Mustn't Leave a Trail", :the-horde "Appear dead", :someone-else "They cause violence and tragedy by accident"},
   [:clubs 8]         {:rank 8, :suit :clubs, :reflect-on "Bargaining", :encounter-something "Everyone", :establish-something "We Can't Fit ", :the-horde "Learn the pattern", :someone-else "They deliver violence and tragedy deliberately"},
   [:spades :jack]    {:rank :jack, :suit :spades, :reflect-on "If Someone Else Will Kill You", :encounter-something "Dark Revelations", :establish-something "We Mustn't Become Monsters", :the-horde "Are a lover", :someone-else "They cause violence and tragedy by accident"},
   [:clubs 6]         {:rank 6, :suit :clubs, :reflect-on "Blame the System", :encounter-something "Freeze", :establish-something "We Can't Reach", :the-horde "Crack the skull", :someone-else "They deliver violence and tragedy deliberately"},
   [:diamonds 6]      {:rank 6, :suit :diamonds, :reflect-on "Blame the Rich", :encounter-something "Fight", :establish-something "We Have A Moment", :the-horde "Snap the spine", :someone-else "They are saved or protected by someone else"},
   [:spades 4]        {:rank 4, :suit :spades, :reflect-on "What Could Stop Them", :encounter-something "An Execution", :establish-something "We Mustn't Say That", :the-horde "Learn something new", :someone-else "They cause violence and tragedy by accident"},
   [:diamonds :ace]   {:rank :ace, :suit :diamonds, :reflect-on "Your Status in this Group", :encounter-something "Explosions", :establish-something "We Have a Special Skill", :the-horde "Are toxic", :someone-else "They are saved or protected by someone else"},
   [:clubs 2]         {:rank 2, :suit :clubs, :reflect-on "What I Hide", :encounter-something "Bells", :establish-something "We Can't Breathe", :the-horde "Break the doors", :someone-else "They deliver violence and tragedy deliberately"},
   [:clubs 9]         {:rank 9, :suit :clubs, :reflect-on "Our Society", :encounter-something "Weeping", :establish-something "We Can't Stop", :the-horde "Frighten animals", :someone-else "They deliver violence and tragedy deliberately"},
   [:diamonds 4]      {:rank 4, :suit :diamonds, :reflect-on "What They Want", :encounter-something "A Murder", :establish-something "We Have a Rope", :the-horde "Cut the power", :someone-else "They are saved or protected by someone else"},
   [:diamonds 3]      {:rank 3, :suit :diamonds, :reflect-on "The Present", :encounter-something "Gouges", :establish-something "We Have A Knife", :the-horde "Just won't die", :someone-else "They are saved or protected by someone else"},
   [:hearts 8]        {:rank 8, :suit :hearts, :reflect-on "Denial", :encounter-something "Noone", :establish-something "We Need Water", :the-horde "Become sentient", :someone-else "They put themselves at risk helping others"},
   [:clubs 5]         {:rank 5, :suit :clubs, :reflect-on "Doctors", :encounter-something "Destruction", :establish-something "We Can't Swim", :the-horde "Eat flesh", :someone-else "They deliver violence and tragedy deliberately"},
   [:hearts 3]        {:rank 3, :suit :hearts, :reflect-on "The Past", :encounter-something "Gashes", :establish-something "We Need Barricades", :the-horde "Still crawl forward", :someone-else "They put themselves at risk helping others"},
   [:diamonds 10]     {:rank 10, :suit :diamonds, :reflect-on "This is Unfair Punishment", :encounter-something "Germs", :establish-something "We Have a Choice To Make", :the-horde "Torture to control", :someone-else "They are saved or protected by someone else"},
   [:spades 3]        {:rank 3, :suit :spades, :reflect-on "The End ", :encounter-something "Guts", :establish-something "We Mustn't Trust Others", :the-horde "Ignore the blow", :someone-else "They cause violence and tragedy by accident"},
   [:clubs 7]         {:rank 7, :suit :clubs, :reflect-on "Where I Might Have Been", :encounter-something "Becoming God", :establish-something "We Can't Help Them", :the-horde "Surround us", :someone-else "They deliver violence and tragedy deliberately"}})

(defn interpret-draw
  [{:keys [active-player players-by-rank]}
   {:keys [rank suit] :as card}]
  (let [{:keys [reflect-on
                encounter-something
                establish-something
                the-horde
                someone-else]} (lookup-card prompts card)
        {:keys [dead? id]}     active-player

        {{:keys [title]} :character
         :as             drawn-char} (get players-by-rank rank active-player)
        other?                       (not= (:id drawn-char) id)]
    (cond
      dead?  (str "You are dead. Describe the ever constant threat to the group. The horde... **" the-horde "**")
      other? (str "You drew _" title " _. Describe how they... **" someone-else "**")
      :else
      (str "Choose one of the following and use it to add to the story:\n\n"
           "* Reflect on... **" reflect-on "**\n"
           "* The group encounters... **" encounter-something "**\n"
           "* Establish important details... **" establish-something "**\n"
           ))))

(defn build-active-card [{:keys [players-by-id
                                 act
                                 paused?
                                 active-player]
                          :as   game-state}
                         {:keys [text type]
                          :as   card}]
  (let [survivors           (remove :dead? (vals players-by-id))
        all-dead?           (empty? survivors)
        active-player-dead? (:dead? active-player)
        new-card            (assoc card
                        :type (or type :prompt)
                        :text (or text (interpret-draw game-state card)))]
    {:card          new-card
     :extra-actions [(if paused? unpause-game-action
                         pause-game-action)
                     leave-game-action]
     :actions       (cond
                      all-dead?                           [lose-game-action]
                      (and (> act 3) active-player-dead?) [lose-game-action]
                      (> act 3)                           [win-game-action]
                      :else                               [done-action]
                      )}))

(defn build-inactive-card [{:keys [players-by-rank
                                   act
                                   paused?
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
     :extra-actions [(if paused? unpause-game-action
                         pause-game-action)
                     leave-game-action]}))

(def characters-list {:ace   {:title       "The child"
                         :description "full of hope"}
                 2      {:title       "The lover"
                         :description "of another character"}
                 3      {:title       "The teacher"
                         :description "building hope"}
                 4      {:title       "The doctor"
                         :description "helping others"}
                 5      {:title       "The soldier"
                         :description "armed and ready"}
                 6      {:title       "The scientist"
                         :description "who knows"}
                 7      {:title       "The celebrity"
                         :description "loved by all"}
                 8      {:title       "The pariah"
                         :description "hated or feared"}
                 9      {:title       "The leader"
                         :description "important to society"}
                 10     {:title       "The millionaire"
                         :description "powerful and rich"}
                 :jack  {:title       "The artist"
                         :description "who can tell the story"}
                 :queen {:title       "The average"
                         :description "an everyday person"}
                 :king  {:title       "The criminal"
                         :description "armed and anxious"}})

(def introduction ["The Walking Deck is a story game played by reading and responding to prompts.\n\nRead these prompts to everyone and when you have added your own details to the story, press \"**Finish Turn**\""
                   "Each player will be introduced as a character of a group of survivors in a zombie wasteland."
                   "The exact nature of the disaster is up to the players."])

(def intro-cards (mapv #(hash-map :text % :type :intro) introduction))
(def padding-card {:text "Finish turn if you are ready to play" :type :intro})

(defn character-card [{:keys [rank] :as card} {:keys [user-name]}]
  (let [{:keys [title description]} (get characters-list rank)]
    (merge card
           {:text (str user-name " is...\n\n" title "... " description)
            :type :character})))

(defn shuffle-discard [discard]
  (->> discard
       (filter #(= :prompt (:type %)))
       shuffle
       (map #(select-keys % [:rank :suit :type]))
       (into [])))

(defn act-timer! [room-id]
  (if (= room-id "fast")
    (* 17 6)
    (* 17 60)))

(defn drama-timer! [room-id player-count]
  (let [ticks (if (= room-id "fast")
                6
                60)]
    (cond
      (< 5 player-count) (* 5 ticks)
      (< 3 player-count) (* 6 ticks)
      (< 1 player-count) (* 8 ticks)
      ;; This shouldn't be possible
      :else (* 8 ticks))
    ))

(defn start-game [world-atom room-id]
  (let [players           (get-in @world-atom [:rooms room-id :players])
        first-player      (first players)
        next-players      (rest players)
        player-count      (count players)
        intro-cards       (->> intro-cards
                               (partition player-count player-count (cycle [padding-card]))
                               (apply concat))
        deck              (shuffle playing-cards)
        [characters deck] (split-at player-count deck)
        character-cards   (map character-card characters players)
        ;; Update the players to assign characters
        players           (map #(assoc %1 :character (merge %2 (get characters-list (:rank %2)))) players characters)
        player-ranks      (zipmap (map :rank characters) players)

        deck              (concat intro-cards character-cards deck)

        new-game          {:players-by-id   (zipmap (map :id players) players)
                           :players-by-rank player-ranks
                           :game-type       :walking-deck
                           :room-id         room-id
                           :act             1
                           :act-prompt      (act-prompts 1)
                           :act-timer       (act-timer! room-id)
                           :drama-timer     (drama-timer! room-id player-count)
                           :discard         []
                           :deck            (rest deck)
                           :active-player   (first players)
                           :next-players    (rest players)}
        new-game          (assoc new-game
                                 :active-display   (build-active-card new-game (first deck))
                                 :inactive-display (build-inactive-card new-game nil))]
    (doto world-atom
      (swap! update-in [:rooms room-id] assoc :game new-game))))

(def death-card {:type :death
                 :text "A fatal encounter occurs. **You are now dead (if you weren't already).** Describe the escalating struggles for the remaining players."})

(def end-game-card {:type :win?
                    :text "After a final climatic attack, any surviving players make it out alive.\n\nAs the credits roll, feel free to describe their fates, or leave it uncertain."})

(def dead-end-game-card {:type :lose?
                    :text "After a final climatic attack, nobody is left standing.\n\nDescribe the last stand of the players as they die"})

(def all-dead-card {:type :lose?
                    :text "Everyone has died."})

(defn finish-card [game]
  (let [{:keys [player-order
                active-player
                next-players
                players-by-id
                discard
                deck
                act]}      game
        active-card        (get-in game [:active-display :card])
        all-players        (conj (into [] next-players) active-player)
        next-up            (first all-players)
        ;; This lets us push first player back in the mix (only single player)
        next-players       (rest all-players)
        survivors          (remove :dead? (vals players-by-id))
        all-dead?          (empty? survivors)
        next-player-alive? (:dead? next-up)
        next-card          (cond
                             all-dead?                          all-dead-card
                             (and (> act 3) next-player-alive?) end-game-card
                             (> act 3)                          dead-end-game-card
                             :else                              (first deck))
        [discard deck]     (if (empty? (rest deck))
                               (do
                                 (println "Shuffling...")
                                 [[active-card] (shuffle-discard discard)])
                               [(cons active-card discard) (into [] (rest deck))])
        next-game          (assoc game
                                    :deck deck
                                    :next-players next-players
                                    :discard discard
                                    :active-player next-up)]
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
        next-card       (first deck)
        [discard deck] (if (empty? (rest deck))
                         [[active-card] (shuffle-discard discard)]
                         [(cons active-card discard) (into [] (rest deck))])
        next-game       (-> game
                            (assoc-in [:inactive-display :x-card-active?] false)
                            (assoc :deck deck
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

(defn pause-game [{:keys [:active-display] :as game}]
  (let [game (assoc game :paused? true)]
    (-> game
        (assoc :active-display (build-active-card game (:card active-display)))
        (assoc :inactive-display (build-inactive-card game nil))
        )))

(defn unpause-game [{:keys [:active-display] :as game}]
  (let [game (assoc game :paused? false)]
    (-> game
        (assoc :active-display (build-active-card game (:card active-display)))
        (assoc :inactive-display (build-inactive-card game nil))
        )))

(defn end-game [game]
  nil)

(defn kill-active-player?
  [{:keys [act active-player players-by-id] :as game}]
  (let [player-id (:id active-player)
        currently-dead? (:dead? active-player)]
    (cond
      (and (> act 3) (not currently-dead?))
      ;; If the current player is alive at end of act 3 (act = 4)
      (-> game
          (assoc :active-display (build-active-card game end-game-card)))
      (> act 3)
      ;; If the current player is dead at end of act 3 (act = 4)
      (-> game
          (assoc :active-display (build-active-card game dead-end-game-card)))
      :else
      (-> game
         (assoc-in [:players-by-id player-id :dead?] true)
         (assoc-in [:active-player :dead?] true)
         (assoc :active-display (build-active-card game death-card))))))

(defn tick-clock [game]
  (let [{:keys [act
                act-timer
                drama-timer
                active-display
                paused?
                room-id
                players-by-id]} game
        player-count            (count players-by-id)
        survivors               (remove :dead? (vals players-by-id))
        new-act-timer           (if (>= 1 act-timer)
                                  (act-timer! room-id)
                                  (dec act-timer))
        new-drama-timer         (if (>= 1 drama-timer)
                                  (drama-timer! room-id player-count)
                                  (dec drama-timer))
        new-act?                (>= 1 act-timer)
        potential-death?        #(if (or (>= 1 drama-timer) new-act?)
                                   (kill-active-player? %)
                                   %)
        all-dead?               (empty? survivors)
        next-act                (if new-act?
                                  (inc act)
                                  act)
        next-act-prompt         (act-prompts next-act)
        prompt-card?            (#{:prompt :death} (get-in active-display [:card :type]))
        ]
    (cond
      (not prompt-card?) game ;; Game not technically started yet, or paused
      (> act 3)          game
      paused?            game
      all-dead?          game
      :else
      (-> game
          (assoc-in [:act-timer] new-act-timer)
          (assoc-in [:act] next-act)
          (assoc-in [:act-prompt] next-act-prompt)
          (assoc-in [:drama-timer] new-drama-timer)
          potential-death?))))

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
                         :pause-game     (pause-game game)
                         :unpause-game   (unpause-game game)
                         :tick-clock     (tick-clock game)
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

  (start-game (atom fake-state) 1))
