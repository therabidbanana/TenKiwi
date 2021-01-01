(ns tenkiwi.gamemasters.walking-deck
  "This game master runs a Walking Deck game"
  (:require [tenkiwi.util :as util :refer [inspect]]))

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

(def pass-action
  {:action :done
   :text   "[Pass] \"Oh my god, we're all going to die!\""})

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

(defn- normalize-card-info [row-num map]
  (-> map
      (update :rank normalize-rank)
      (update :suit normalize-suit)))

(defn- to-lookup-map [card-rows]
  (into {}
        (for [{:keys [rank suit]
               :as row} card-rows]
          [[suit rank] row])))

(comment
  ;; Use eval and replace to pull in a published sheet with tsv
  ;; Use :s/, \[/,\n\]/g
  ;; (read-spreadsheet-data url)
  (to-lookup-map (util/read-spreadsheet-data "https://docs.google.com/spreadsheets/d/e/2PACX-1vQBY3mq94cg_k3onDKmA1fa_L3AGbKVBfdxxeP04l73QVIXMkD4gEdG-e2ciex2jjTJjaKkdU1Vtaf1/pub?gid=481445422&single=true&output=tsv"
                                             normalize-card-info)
                 ))

(def prompts
  {[:clubs :jack]     {:suit :clubs, :encounter-something "Purgatory", :the-horde "Are a friend", :reflect-on "If Someone Else Will Die", :rank :jack, :the-card-falls "Malefactor", :establish-something "You Can't Close It", :character-action "They deliver violence and tragedy deliberately", :character "The artist, telling the story"},
   [:spades 9]        {:suit :spades, :encounter-something "Whispering", :the-horde "Are full of animals", :reflect-on "Our Collective Beliefs", :rank 9, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Reveal Ourselves", :character-action "They cause violence and tragedy by accident", :character "The leader, important to society"},
   [:diamonds 2]      {:suit :diamonds, :encounter-something "Alarms", :the-horde "Get inside", :reflect-on "What You Truly Want", :rank 2, :the-card-falls "Saved By Another", :establish-something "You Have A Gun", :character-action "They are saved or protected by someone else", :character "The lover, of another character"},
   [:hearts :ace]     {:suit :hearts, :encounter-something "Smoke", :the-horde "Are on fire", :reflect-on "Your Place in Society", :rank :ace, :the-card-falls "Saviour of Another", :establish-something "You Need Transport", :character-action "They put themselves at risk helping others", :character "The child, full of innocence"},
   [:spades 8]        {:suit :spades, :encounter-something "A Loved One", :the-horde "Show low cunning", :reflect-on "Depression", :rank 8, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Get Hurt", :character-action "They cause violence and tragedy by accident", :character "The pariah, feared and hated"},
   [:clubs :ace]      {:suit :clubs, :encounter-something "Ashes", :the-horde "Are smelly", :reflect-on "You Being Betrayed By Others", :rank :ace, :the-card-falls "Malefactor", :establish-something "You Can't Find It", :character-action "They deliver violence and tragedy deliberately", :character "The child, full of innocence"},
   [:spades 10]       {:suit :spades, :encounter-something "Stone", :the-horde "Torture for fun", :reflect-on "This is Without Meaning ", :rank 10, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Give In", :character-action "They cause violence and tragedy by accident", :character "The millionaire, used to power"},
   [:hearts :queen]   {:suit :hearts, :encounter-something "Death", :the-horde "Appear innocent", :reflect-on "Who Is Coming to Save You", :rank :queen, :the-card-falls "Saviour of Another", :establish-something "You Need Order", :character-action "They put themselves at risk helping others", :character "The nobody, just average "},
   [:diamonds :queen] {:suit :diamonds, :encounter-something "Dying", :the-horde "Appear helpful", :reflect-on "Who Isn't Coming to Save You", :rank :queen, :the-card-falls "Saved By Another", :establish-something "You Have a Crowbar", :character-action "They are saved or protected by someone else", :character "The nobody, just average "},
   [:diamonds :jack]  {:suit :diamonds, :encounter-something "A Fresh Hell", :the-horde "Are an enemy", :reflect-on "If You'll Die", :rank :jack, :the-card-falls "Saved By Another", :establish-something "You Have A Tactical Edge", :character-action "They are saved or protected by someone else", :character "The artist, telling the story"},
   [:clubs :king]     {:suit :clubs, :encounter-something "Debasement", :the-horde "Ride the bus", :reflect-on "The Last Thing You'll Ever See", :rank :king, :the-card-falls "Malefactor", :establish-something "You Can't Control It", :character-action "They deliver violence and tragedy deliberately", :character "The criminal, armed and dangerous"},
   [:hearts 10]       {:suit :hearts, :encounter-something "Guns", :the-horde "Torture to betrayal", :reflect-on "This is Fair Punishment", :rank 10, :the-card-falls "Saviour of Another", :establish-something "You Need A Way Out", :character-action "They put themselves at risk helping others", :character "The millionaire, used to power"},
   [:spades 2]        {:suit :spades, :encounter-something "Silence", :the-horde "Almost reach", :reflect-on "What I'm Doing", :rank 2, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Be Heard", :character-action "They cause violence and tragedy by accident", :character "The lover, of another character"},
   [:diamonds 9]      {:suit :diamonds, :encounter-something "Yelling", :the-horde "Control animals", :reflect-on "Our Collective Culture", :rank 9, :the-card-falls "Saved By Another", :establish-something "You Have A Cellphone", :character-action "They are saved or protected by someone else", :character "The leader, important to society"},
   [:hearts 7]        {:suit :hearts, :encounter-something "Praising God", :the-horde "Overwhelm us", :reflect-on "Where I Should Be Instead", :rank 7, :the-card-falls "Saviour of Another", :establish-something "You Need Food", :character-action "They put themselves at risk helping others", :character "The celebrity, loved by all"},
   [:clubs 10]        {:suit :clubs, :encounter-something "Steel", :the-horde "Torture for fear", :reflect-on "This is A Blessing in Disguise", :rank 10, :the-card-falls "Malefactor", :establish-something "You Can't Open It", :character-action "They deliver violence and tragedy deliberately", :character "The millionaire, used to power"},
   [:diamonds 8]      {:suit :diamonds, :encounter-something "Someone", :the-horde "Form a strategy", :reflect-on "Anger", :rank 8, :the-card-falls "Saved By Another", :establish-something "You Have a Contact", :character-action "They are saved or protected by someone else", :character "The pariah, feared and hated"},
   [:diamonds 7]      {:suit :diamonds, :encounter-something "Fearing God", :the-horde "Press in on us", :reflect-on "Where I Could Be Instead", :rank 7, :the-card-falls "Saved By Another", :establish-something "You Have a Promise", :character-action "They are saved or protected by someone else", :character "The celebrity, loved by all"},
   [:clubs :queen]    {:suit :clubs, :encounter-something "Aging", :the-horde "Appear harmless", :reflect-on "Who Might Come to Silence You", :rank :queen, :the-card-falls "Malefactor", :establish-something "You Can't Carry It", :character-action "They deliver violence and tragedy deliberately", :character "The nobody, just average "},
   [:spades 6]        {:suit :spades, :encounter-something "Fawn", :the-horde "Break the bone", :reflect-on "Blaming Ourselves", :rank 6, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Miss Our Chance", :character-action "They cause violence and tragedy by accident", :character "The scientist, who knows something"},
   [:spades :king]    {:suit :spades, :encounter-something "Desecration", :the-horde "Crash cars", :reflect-on "Your Last Dying Wish", :rank :king, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Be Too Late", :character-action "They cause violence and tragedy by accident", :character "The criminal, armed and dangerous"},
   [:spades 5]        {:suit :spades, :encounter-something "Falling", :the-horde "Drool and vomit", :reflect-on "If Journalists Are Reporting On This", :rank 5, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Be Left Behind", :character-action "They cause violence and tragedy by accident", :character "The soldier, armed and ready"},
   [:hearts 2]        {:suit :hearts, :encounter-something "Sirens", :the-horde "Grab someone", :reflect-on "Who I Really Am", :rank 2, :the-card-falls "Saviour of Another", :establish-something "You Need Bandages", :character-action "They put themselves at risk helping others", :character "The lover, of another character"},
   [:hearts 6]        {:suit :hearts, :encounter-something "Flight", :the-horde "Suck the juices", :reflect-on "Blaming the Military", :rank 6, :the-card-falls "Saviour of Another", :establish-something "You Need More Bullets", :character-action "They put themselves at risk helping others", :character "The scientist, who knows something"},
   [:hearts :king]    {:suit :hearts, :encounter-something "Defiance", :the-horde "Down airplanes", :reflect-on "The Last People Alive", :rank :king, :the-card-falls "Saviour of Another", :establish-something "You Need Teamwork", :character-action "They put themselves at risk helping others", :character "The criminal, armed and dangerous"},
   [:hearts 5]        {:suit :hearts, :encounter-something "Shattering", :the-horde "Swarm like locusts", :reflect-on "If Cops Can Help Us", :rank 5, :the-card-falls "Saviour of Another", :establish-something "You Need Information", :character-action "They put themselves at risk helping others", :character "The soldier, armed and ready"},
   [:hearts :jack]    {:suit :hearts, :encounter-something "A Kind of Heaven", :the-horde "Are a relative", :reflect-on "If You'll Live", :rank :jack, :the-card-falls "Saviour of Another", :establish-something "You Need A Leader", :character-action "They put themselves at risk helping others", :character "The artist, telling the story"},
   [:clubs 4]         {:suit :clubs, :encounter-something "A Judgement", :the-horde "Find weapons", :reflect-on "Where This Is Leading", :rank 4, :the-card-falls "Malefactor", :establish-something "You Can't Get Out", :character-action "They deliver violence and tragedy deliberately", :character "The doctor, helping others"},
   [:diamonds 5]      {:suit :diamonds, :encounter-something "Collapsing", :the-horde "Devour the still alive", :reflect-on "What Has Science Done", :rank 5, :the-card-falls "Saved By Another", :establish-something "You Have A Pile of Cash", :character-action "They are saved or protected by someone else", :character "The soldier, armed and ready"},
   [:hearts 4]        {:suit :hearts, :encounter-something "A Corpse", :the-horde "Take control", :reflect-on "Where They Came From", :rank 4, :the-card-falls "Saviour of Another", :establish-something "You Need Drugs", :character-action "They put themselves at risk helping others", :character "The doctor, helping others"},
   [:spades :ace]     {:suit :spades, :encounter-something "Fire", :the-horde "Are everywhere", :reflect-on "Your Chance of Survival", :rank :ace, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Follow Orders", :character-action "They cause violence and tragedy by accident", :character "The child, full of innocence"},
   [:hearts 9]        {:suit :hearts, :encounter-something "Screaming", :the-horde "Infest animals", :reflect-on "Our Troubled Nation", :rank 9, :the-card-falls "Saviour of Another", :establish-something "You Need To Rest", :character-action "They put themselves at risk helping others", :character "The leader, important to society"},
   [:diamonds :king]  {:suit :diamonds, :encounter-something "Deference", :the-horde "Derail the train", :reflect-on "The Last Place To Hide", :rank :king, :the-card-falls "Saved By Another", :establish-something "You Have a Bible", :character-action "They are saved or protected by someone else", :character "The criminal, armed and dangerous"},
   [:clubs 3]         {:suit :clubs, :encounter-something "Gorging", :the-horde "Get back up", :reflect-on "The Future of Humanity", :rank 3, :the-card-falls "Malefactor", :establish-something "You Can't See", :character-action "They deliver violence and tragedy deliberately", :character "The teacher, building hope"},
   [:spades 7]        {:suit :spades, :encounter-something "Killing God", :the-horde "Come from behind", :reflect-on "Where I Will Never Be", :rank 7, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Believe Them", :character-action "They cause violence and tragedy by accident", :character "The celebrity, loved by all"},
   [:spades :queen]   {:suit :spades, :encounter-something "Rebirth", :the-horde "Appear dead", :reflect-on "Who Has Abandoned You", :rank :queen, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Leave a Trail", :character-action "They cause violence and tragedy by accident", :character "The nobody, just average "},
   [:clubs 8]         {:suit :clubs, :encounter-something "Everyone", :the-horde "Learn the pattern", :reflect-on "Bargaining", :rank 8, :the-card-falls "Malefactor", :establish-something "You Can't Fit ", :character-action "They deliver violence and tragedy deliberately", :character "The pariah, feared and hated"},
   [:spades :jack]    {:suit :spades, :encounter-something "Revelations", :the-horde "Are a lover", :reflect-on "If Someone Else Will Kill You", :rank :jack, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Become Monsters", :character-action "They cause violence and tragedy by accident", :character "The artist, telling the story"},
   [:clubs 6]         {:suit :clubs, :encounter-something "Freeze", :the-horde "Crack the skull", :reflect-on "Blaming the System", :rank 6, :the-card-falls "Malefactor", :establish-something "You Can't Reach", :character-action "They deliver violence and tragedy deliberately", :character "The scientist, who knows something"},
   [:diamonds 6]      {:suit :diamonds, :encounter-something "Fight", :the-horde "Snap the spine", :reflect-on "Blaming the Rich", :rank 6, :the-card-falls "Saved By Another", :establish-something "You Have A Moment", :character-action "They are saved or protected by someone else", :character "The scientist, who knows something"},
   [:spades 4]        {:suit :spades, :encounter-something "An Execution", :the-horde "Learn something new", :reflect-on "What Could Stop Them", :rank 4, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Say That", :character-action "They cause violence and tragedy by accident", :character "The doctor, helping others"},
   [:diamonds :ace]   {:suit :diamonds, :encounter-something "Explosions", :the-horde "Are toxic", :reflect-on "Your Status in this Group", :rank :ace, :the-card-falls "Saved By Another", :establish-something "You Have a Special Skill", :character-action "They are saved or protected by someone else", :character "The child, full of innocence"},
   [:clubs 2]         {:suit :clubs, :encounter-something "Bells", :the-horde "Break the doors", :reflect-on "What Secrets You Hide", :rank 2, :the-card-falls "Malefactor", :establish-something "You Can't Breathe", :character-action "They deliver violence and tragedy deliberately", :character "The lover, of another character"},
   [:clubs 9]         {:suit :clubs, :encounter-something "Weeping", :the-horde "Frighten animals", :reflect-on "Our Collective Society", :rank 9, :the-card-falls "Malefactor", :establish-something "You Can't Stop", :character-action "They deliver violence and tragedy deliberately", :character "The leader, important to society"},
   [:diamonds 4]      {:suit :diamonds, :encounter-something "A Murder", :the-horde "Cut the power", :reflect-on "What They Want", :rank 4, :the-card-falls "Saved By Another", :establish-something "You Have a Rope", :character-action "They are saved or protected by someone else", :character "The doctor, helping others"},
   [:diamonds 3]      {:suit :diamonds, :encounter-something "Gouges", :the-horde "Just won't die", :reflect-on "RIght Here, Right Now", :rank 3, :the-card-falls "Saved By Another", :establish-something "You Have An Axe", :character-action "They are saved or protected by someone else", :character "The teacher, building hope"},
   [:hearts 8]        {:suit :hearts, :encounter-something "Noone", :the-horde "Become sentient", :reflect-on "Denial", :rank 8, :the-card-falls "Saviour of Another", :establish-something "You Need Water", :character-action "They put themselves at risk helping others", :character "The pariah, feared and hated"},
   [:clubs 5]         {:suit :clubs, :encounter-something "Destruction", :the-horde "Eat flesh", :reflect-on "If Doctors Can Helo", :rank 5, :the-card-falls "Malefactor", :establish-something "You Can't Swim", :character-action "They deliver violence and tragedy deliberately", :character "The soldier, armed and ready"},
   [:hearts 3]        {:suit :hearts, :encounter-something "Gashes", :the-horde "Still crawl forward", :reflect-on "The Past That Led Here", :rank 3, :the-card-falls "Saviour of Another", :establish-something "You Need Barricades", :character-action "They put themselves at risk helping others", :character "The teacher, building hope"},
   [:diamonds 10]     {:suit :diamonds, :encounter-something "Germs", :the-horde "Torture to control", :reflect-on "This is Unfair Punishment", :rank 10, :the-card-falls "Saved By Another", :establish-something "You Have a Choice To Make", :character-action "They are saved or protected by someone else", :character "The millionaire, used to power"},
   [:spades 3]        {:suit :spades, :encounter-something "Guts", :the-horde "Ignore the blow", :reflect-on "The End ", :rank 3, :the-card-falls "Fortune's Fool", :establish-something "You Mustn't Trust Others", :character-action "They cause violence and tragedy by accident", :character "The teacher, building hope"},
   [:clubs 7]         {:suit :clubs, :encounter-something "Becoming God", :the-horde "Surround us", :reflect-on "Where I Might Have Been", :rank 7, :the-card-falls "Malefactor", :establish-something "You Can't Help Them", :character-action "They deliver violence and tragedy deliberately", :character "The celebrity, loved by all"}}
  )

(defn- card-name
  ([{:keys [rank suit]}]
   (card-name rank suit))
  ([rank suit]
   (let [rank (cond (keyword? rank) (clojure.string/capitalize (name rank))
                    :else rank)
         suit (clojure.string/capitalize (name suit))]
     (str "the " rank " of " suit))))

(defn interpret-draw
  [{:keys [active-player next-players]}
   {:keys [rank suit] :as card}]
  (let [{:keys [reflect-on
                encounter-something
                establish-something
                the-horde
                character-action]} (lookup-card prompts card)
        all-players            (cons active-player next-players)
        {:keys [dead? id]}     active-player
        matching-players       (keep #(if (= rank (get-in % [:character :rank]))
                                        %
                                        nil)
                                     all-players)

        {{:keys [title]} :character
         :keys           [id]
         :as             drawn-char} (first matching-players)
        dead-draw?                   (if drawn-char
                     (:dead? drawn-char)
                     dead?)
        card-title (card-name card)
        ]
    (cond
      dead-draw?
      (str "You drew _" (card-name card) "_. "
           "Choose one of the following to add to the story:\n\n"
           "* The horde... **" the-horde "**\n"
           "* Establish important details... **" establish-something "**\n"
           )
      drawn-char
      (str "You drew _" title "_. Describe how they... **" character-action "**"
           )
      :else
      (str "You drew _" (card-name card) "_. "
           "Choose one of the following and use it to add to the story:\n\n"
           "* Reflect on... **" reflect-on "**\n"
           "* The group encounters... **" encounter-something "**\n"
           ))))

(defn build-active-card [{:keys [act
                                 paused?
                                 active-player
                                 next-players]
                          :as   game-state}
                         {:keys [text type]
                          :as   card}]
  (let [all-players         (cons active-player next-players)
        survivors           (remove :dead? all-players)
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
                      (#{:prompt} (:type new-card))       [done-action pass-action]
                      :else                               [done-action]
                      )}))

(defn build-inactive-card [{:keys [act
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
      ;; This shouldn't be possible unless a person is playing alone
      :else (* 8 ticks))
    ))

(defn prepare-deck [count]
  (let [deck (shuffle playing-cards)]
    ))

(defn start-game [world-atom room-id params]
  (let [extra-players     (get params :extra-players 0)
        original-players  (get-in @world-atom [:rooms room-id :players])
        players           (take (+ extra-players (count original-players))
                                (cycle original-players))
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

        deck (concat intro-cards character-cards deck)

        new-game {:game-type       :walking-deck
                  :room-id         room-id
                  :act             1
                  :act-prompt      (act-prompts 1)
                  :act-timer       (act-timer! room-id)
                  :drama-timer     (drama-timer! room-id player-count)
                  :discard         []
                  :deck            (rest deck)
                  :active-player   (first players)
                  :next-players    (rest players)}
        new-game (assoc new-game
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
                discard
                deck
                act]}      game
        active-card        (get-in game [:active-display :card])
        all-players        (conj (into [] next-players) active-player)
        next-up            (first all-players)
        ;; This lets us push first player back in the mix (only single player)
        next-players       (rest all-players)
        survivors          (remove :dead? all-players)
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
  [{:keys [act active-player] :as game}]
  (let [currently-dead? (:dead? active-player)]
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
         (assoc-in [:active-player :dead?] true)
         (assoc :active-display (build-active-card game death-card))))))

(defn tick-clock [game]
  (let [{:keys [act
                act-timer
                drama-timer
                active-display
                active-player
                next-players
                paused?
                room-id]} game
        all-players             (cons active-player next-players)
        player-count            (count all-players)
        survivors               (remove :dead? all-players)
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
