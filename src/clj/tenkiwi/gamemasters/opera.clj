(ns tenkiwi.gamemasters.opera
  "This is the game logic for opera game"
  (:require
   [clojure.string :as string]
   [tenkiwi.tables.debrief :as tables]
   [tenkiwi.util :as util :refer [inspect]]))

(def valid-active-actions #{:upstress-player :downstress-player :jump-ahead :regen :pass :discard :undo :done :x-card :end-game :leave-game})
(def valid-inactive-actions #{:upstress-player :downstress-player :jump-ahead :x-card :undo :leave-game})

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

(def jump-ahead-action
  {:action  :jump-ahead
   :confirm true
   :text    "Make Theory"})

(def regen-action
  {:action :regen
   :params {}
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

(def undo-action
  {:action  :undo
   :text    "Undo Last"})

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
                     :as   game}
                    card]
  (let [next-player   (:id (next-player player-order active-player))
        prev-player   (:id (previous-player player-order active-player))
        secondaries   (:secondary-objectives mission [])
        complications (:complications mission [])
        scores        (score-players game)
        values        (:values company)]
    {:leader-name    (get-in game [:dossiers :leader :agent-codename] "")
     :player-left    (get-in game [:dossiers prev-player :agent-codename] "")
     :player-right   (get-in game [:dossiers next-player :agent-codename] "")
     :value-0        (nth values 0 "unknown")
     :value-1        (nth values 1 "unknown")
     :value-2        (nth values 2 "unknown")
     :primary        (:primary-objective mission)
     :secondary      (first (shuffle secondaries))
     :secondary-0    (nth secondaries 0 "unknown")
     :secondary-1    (nth secondaries 1 "unknown")
     :secondary-2    (nth secondaries 2 "unknown")
     :complication   (-> card :scene :text)
     :complication-0 (nth complications 0 "unknown")
     :complication-1 (nth complications 1 "unknown")
     :complication-2 (nth complications 2 "unknown")
     :focus          (-> card :scene :text)
     :focus-type     (case (-> card :scene :type)
                       :clue "Clue"
                       :complication "Complication"
                       "Free")
     :npc            (-> card :scene :npc)
     :setting        (-> card :scene :setting)
     }))

(defn replace-vars [game str-or-card]
  (let [text      (if (string? str-or-card)
                    str-or-card
                    (:text str-or-card))
        game-vars (extract-vars game str-or-card)
        replaced  (string/replace (or text "")
                                          #"\{([^\}]+)\}"
                                          #(get game-vars (keyword (nth % 1))
                                                (nth % 1)))]
    (cond
      (string? str-or-card)
      replaced
      (map? str-or-card)
      (assoc str-or-card :text replaced)
      :else str-or-card)))

(defn extract-generator-list [str]
  (->> (clojure.string/split str #"\s\s")
       (map #(clojure.string/split % #":\s+"))
       (map #(hash-map :name (first %)
                       :label (last %)))))

;; TODO: Use above thing
(defn dossier-card
  ([dossier-template generators player]
   (dossier-card dossier-template generators player {}))
  ([dossier-template generators player defaults]
   (let [generator-list (->> (clojure.string/split (:inputs dossier-template) #"\s\s")
                             (map #(clojure.string/split % #":"))
                             (into {}))
         pluck-value    (fn [keyname]
                          (or (get defaults keyname)
                              (-> generators
                                  (get keyname [{:text "unknown"}])
                                  shuffle
                                  first
                                  :text)))]
     (merge dossier-template
            {:id     :player-dossier
             :inputs (mapv #(hash-map :name (first %)
                                      :label (last %)
                                      :value (pluck-value (first %)))
                           generator-list)}))))

;; TODO: XSS danger?
(defn waiting-for
  [{:keys [user-name]}]
  {:id    "waiting"
   :type :inactive
   :text  (str "It is " user-name "'s turn...")})


(defn build-active-card
  ([game card active-player next-player]
   (let [{:keys [all-players
                 mission
                 -generators]} game
         story-details         (extract-generator-list (:story-details mission ""))
         scene-details         (if (:scene card)
                                 [{:title "Character"
                                    :name "npc"
                                   :items [(-> card :scene :npc)]}
                                  {:title "Setting"
                                   :name "setting"
                                   :items [(-> card :scene :setting)]}]
                                 [])
         act                   (:act card)
         next-stage            (or (:type card) :intro)
         has-scene?            (:scene card)
         starting-player       (or (:starting-player card) active-player)
         ;; Don't allow done-action for #everyone cards until they are passed around
         can-finish?           (or (not (get-in card [:tags :everyone] false))
                                   (= (:starting-player card) active-player))
         pass                  {:action :pass
                                :text   (str "Pass card to " (:user-name next-player))}]
     {:card              (assoc (replace-vars game card)
                                :starting-player starting-player)
      :extra-details     (concat
                          scene-details
                          (map #(hash-map :title (:label %)
                                          :name (:name %)
                                          :items (take 3 (shuffle (mapv :text (get -generators (:name %) [])))))
                               story-details))
      :extra-actions     (if has-scene?
                           [jump-ahead-action undo-action leave-game-action]
                           [undo-action leave-game-action])
      :available-actions valid-active-actions
      :actions           (case next-stage
                           :epilogue-close [pass end-game-action]
                           :dossier        [done-action]
                           (if can-finish?
                             [done-action pass]
                             [pass]))}))
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
     :available-actions valid-inactive-actions
     :extra-actions [undo-action leave-game-action]}))

(defn build-inactive-version [{:keys [dossiers active-player] :as game}
                              {{:keys [type]} :card
                               :as            active-display}]
  (let [disabled-actions [{:text      (:text (waiting-for active-player))
                           :disabled? true}]
        extra-actions    []]
    (cond
      (#{:act-start :question :intro :mission-briefing} type)
      (-> active-display
          (assoc :available-actions valid-inactive-actions)
          (assoc :actions disabled-actions)
          (update :actions #(into % extra-actions)))
      (#{:dossier} type)
      (build-inactive-card active-player "Introductions are being made.")
      :else
      (-> active-display
          (assoc :available-actions valid-inactive-actions)))))

(defn- build-starting-scores [{:keys [npc? id]} players]
  (let [ids (remove #(= id %) (map :id players))]
    (zipmap ids (cycle [5]))))

(defn build-mission-details [{:keys [text group story-details] :as card}
                             {:keys [mission-briefing mission-clock-complication
                                     mission-clue mission-complication
                                     mission-title
                                     mission-npc mission-setting]
                              :as   decks}]
  (let [briefing                    (->> (string/split text #"\n\n")
                                         (map #(hash-map :text % :type :mission-briefing)))
        mission-briefing            (group-by :group mission-briefing)
        mission-title               (-> (group-by :group mission-title) (get group [group]) first)
        mission-open                (get mission-briefing "0" [])
        mission-wrapup              (get mission-briefing "2" [])
        mission-middle              (get mission-briefing group [])
        mission-complications       (-> (group-by :group mission-complication) (get group []))
        mission-clock-complications (-> (group-by :group mission-clock-complication) (get group []))
        mission-clues               (-> (group-by :group mission-clue) (get group []))
        mission-npcs                (-> (group-by :group mission-npc) (get group []))
        mission-settings            (-> (group-by :group mission-setting) (get group []))]
    (-> card
        (assoc :briefing-cards (concat mission-open
                                       mission-middle
                                       mission-wrapup))
        (assoc :text (clojure.string/join "\n\n" (map :text mission-middle)))
        (assoc :id group)
        (assoc :title (:text mission-title group))
        (assoc :clues
               (->> mission-clues
                    shuffle
                    (map (fn [x] (assoc x :type :clue)))))
        (assoc :complications
               (->> mission-complications
                    shuffle
                    (map (fn [x] (assoc x :type :complication)))))
        (assoc :npcs
               (->> (map :text mission-npcs)
                    shuffle
                    (map string/trim)))
        (assoc :clock-complications
               (->> mission-clock-complications
                    (map (fn [x] (assoc x :type :complication)))))
        (assoc :settings
               (->> (map :text mission-settings)
                    shuffle
                    (map string/trim)))
        #_(inspect ))))

(defn extract-missions [{:keys [mission-briefing mission
                                mission-clue mission-complication
                                mission-npc mission-setting]
                         :as decks}]
  (let [missions (mapv #(build-mission-details % decks) mission)]
    [missions
     (dissoc decks
             :mission-briefing
             :mission-complication :mission-clue
             :mission-npc :mission-setting)]))

(defn one-per-group
  ([act-collection]
   (let [grouped (group-by :group act-collection)]
     (zipmap (keys grouped)
             (map first (vals grouped)))))
  ([act-collection func]
   (let [grouped (group-by :group act-collection)]
     (zipmap (keys grouped)
             (map #(-> % first func) (vals grouped))))))

(defn build-scene [focus
                   {:keys [npcs settings] :as mission-details}
                   {:keys [prompt setup] :as decks}]
  (let [scene-type (get focus :type)
        setups   (-> (group-by :group setup)
                     (get (name scene-type)))
        starter  (cond
                   (= :clue scene-type)
                   {:type :scene-open
                    :text "**New Scene**\n\nThis scene will focus on getting a clue: _{focus}_.\n\nWork together using the wordbank behind this card to imagine the scene leading up to that moment, then continue to answer the prompts."}
                   :else
                   {:type :scene-open
                    :text "**New Scene**\n\nThis scene will focus on a complication: _{focus}_.\n\nWork together using the wordbank behind this card to imagine the scene leading up to that moment, then continue to answer the prompts."})

        prompts [{:type :prompt :text "filler"}
                 {:type :prompt :text "filler"}
                 {:type :prompt :text "filler"}]
        ender   (assoc (first (shuffle setups))
                       :type :scene-close)
        focus   (assoc
                 focus
                 :npc (first (shuffle npcs))
                 :setting (first (shuffle settings)))]
    (->> (concat [starter] prompts [ender])
         (map #(assoc % :scene focus))
         (into []))))

(defn build-closing-scenes [{:keys [epilogue setup] :as decks}]
  (let [starter  [{:type :theory
                   :text "Work together to determine what you think the phenomenon might be, how you might contain it, and how you can obfuscate ECB involvement."}
                  {:type :epilogue-open
                   :text "Theory roll here"}]
        prompts [{:type :epilogue :text "filler"} {:type :epilogue :text "filler"}
                 {:type :epilogue :text "filler"}
                 {:type :epilogue :text "filler"} {:type :epilogue :text "filler"}
                 {:type :epilogue :text "filler"} {:type :epilogue :text "filler"}]
        ender   [{:type :epilogue-close
                  :text "The mission has ended. How did it go?"}]]
    (->> (concat starter prompts ender)
         (into []))))

(defn pluck
  ([generators gen-name]
   (first (pluck generators gen-name 1)))
  ([generators gen-name count]
   (->> [{:text "Foo"}]
        (get generators gen-name)
        shuffle
        (take count)
        (map :text))))

(defn select-game [room-id {:keys [game-url mission-id]
                           :as   params
                           :or   {game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQGZTHQnC9oQxhEnSzS-cYkQNTExjW3VVNMOIkvkNpVfEPhB_XwZN9kTMCYguSmksFKdvf1-ExTmKU0/pub?gid=0&single=true&output=tsv"}}
                  {:keys [players] :as room}]
  (let [first-player        (first players)
        next-player         (next-player players (:id first-player))
        decks               (util/gather-decks game-url)
        dossier-template    (->> decks :dossier first)
        [missions decks]    (extract-missions decks)
        mission-details     (or
                              (first (filter #(= (:id %) mission-id) missions))
                              (first (shuffle missions)))

        new-game       {:game-type        :opera
                        :configuration    {:params (assoc params :mission-id (:id mission-details))
                                           :inputs [{:type    :select
                                                     :label   "Mission"
                                                     :name    :mission-id
                                                     :options (mapv #(hash-map :value (:id %) :name (:title %))
                                                                    missions)}
                                                    ]}
                        :mission          mission-details
                        :dossier-template dossier-template}]
    new-game))

(defn start-game [room-id {:keys [game-url mission-id]
                           :as   params
                           :or   {game-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQGZTHQnC9oQxhEnSzS-cYkQNTExjW3VVNMOIkvkNpVfEPhB_XwZN9kTMCYguSmksFKdvf1-ExTmKU0/pub?gid=0&single=true&output=tsv"}}
                  {:keys [players] :as room}]
  (let [first-player        (first players)
        next-player         (next-player players (:id first-player))
        party-npcs          []

        {intro-cards :intro
         prompts     :prompt
         epilogues   :epilogue
         theories    :epilogue-open
         endings     :epilogue-close
         :as         decks} (util/gather-decks game-url)
        setups              (->> decks :setup (group-by :group))
        mission-briefing    (->> decks :mission-briefing (group-by :group))
        generators          (->> decks :generator (group-by :group))
        dossier-template    (->> decks :dossier first)
        [missions decks]    (extract-missions decks)
        mission-details     (or
                              (first (filter #(= (:id %) mission-id) missions))
                              (first (shuffle missions)))

        all-players (concat (into [] players) party-npcs)
        scene-count 8
        scene-focus (interleave (shuffle (:clues mission-details))
                                (concat
                                 (take 1 (shuffle (:complications mission-details)))
                                 (:clock-complications mission-details))
                            #_(:complications mission-details))

        ;; TODO: Drop in clock complications - or do those work into existing scenes?
        investigate-scenes (take scene-count scene-focus)

        company     {:name   (pluck generators "company")
                     :values (pluck generators "value" 3)}
        dossiers    {:leader {:agent-name     (tables/random-name)
                              :agent-codename (pluck generators "leader-codename")
                              :agent-role     (pluck generators "leader-role")}}

        active-display (build-active-card (first intro-cards) first-player next-player)
        new-game       {:player-order     (into [] players)
                        :player-scores    (into {}
                                                (map #(vector (:id %)
                                                              (build-starting-scores % players)) all-players))
                        :all-players      all-players
                        :game-type        :opera
                        :stage-names      {:intro            "Introduction"
                                           :mission-briefing "Mission Briefing"
                                           :dossier          "Character Intros"
                                           :prompt           "{focus-type} - {focus}"
                                           :scene-open       "{focus-type} - {focus}"
                                           :scene-close      "{focus-type} - {focus}"
                                           :theory           "Theorizing"
                                           :epilogue-open       "Resolution"
                                           :epilogue            "Resolution"
                                           :epilogue-close      "Resolution"}
                        :position-tags    {1 [:green]
                                           2 [:green :yellow]
                                           3 [:yellow]
                                           4 [:yellow :red]
                                           5 [:red]}
                        :position         1
                        :clocks           {:plot 1}
                        :stage            :intro
                        :stage-name       "Introduction"
                        :dossiers         dossiers
                        :scenes           []
                        :mission          mission-details
                        :-discard          []
                        :company          company
                        :dossier-template dossier-template
                        :-generators      generators
                        :-prompt-decks    {:prompt (shuffle prompts)
                                           :epilogue-open (shuffle theories)
                                           :epilogue-close (shuffle endings)
                                           :epilogue (shuffle epilogues)}
                        :-deck            (into []
                                                (concat (rest intro-cards)
                                                        (map (partial dossier-card dossier-template generators) players)
                                                        (:briefing-cards mission-details)
                                                        (mapcat #(build-scene % mission-details decks)
                                                                investigate-scenes)
                                                        (build-closing-scenes decks)))
                        :active-player    (first players)
                        :active-display   active-display
                        :inactive-display (build-inactive-version {:active-player (first players)} active-display)}]
    new-game))

(comment
  (let [game (start-game "foo" {} {:players [{:name "David"}]})]
    (:-deck game))

  )

(defn extract-dossier [{:keys [inputs]}]
  (-> (zipmap (map keyword (map :name inputs))
              (map :value inputs))
      (assoc :stress 0)))

(defn get-stage-info [{:keys [company
                              stage-names]
                       :as game}
                      next-card]
  (let [next-stage      (get next-card :type :intro)
        next-act        (clojure.string/replace (str (get next-card :act "0"))
                                                #"[^\d]"
                                                "")
        next-stage-name (->> (get stage-names next-stage "Introduction")
                             (assoc next-card :text)
                             (replace-vars game)
                             :text)]
    {:stage       next-stage
     :stage-name  next-stage-name}))

(defn draw-prompt [next-card tags prompt-decks]
  (let [deck-type             (:type next-card)
        prompts               (get prompt-decks deck-type [])
        [matches non-matches] ((juxt filter remove)
                               #(some (:tags %) tags)
                               (shuffle prompts))
        next-card             (merge next-card (first matches))]
    (if-not (empty? prompts)
      [next-card (assoc prompt-decks deck-type (concat (rest matches) non-matches))]
      [next-card prompt-decks]
      )))

(defn roll-theory [dice]
  (let [rolls (map (fn [i] (inc (rand-int 6))) (range 0 dice))
        result (apply max rolls)]
    (cond (> result 5) ;; GREAT!
          1
          (> result 3) ;; Mixed
          3
          :else ;; Fail
          5)))

(defn finish-card [game]
  (let [{:keys [player-order
                active-player
                dossiers
                scenes
                -prompt-decks
                position-tags
                position
                clocks
                -discard
                -deck
                stage]} game
        active-card     (get-in game [:active-display :card])
        dossiers        (if (#{:player-dossier} (:id active-card))
                          (assoc dossiers (:id active-player)
                                 (extract-dossier active-card))
                          dossiers)
        active-tags     (:tags active-card)
        next-up         (next-player player-order active-player)
        discard         (cons active-card -discard)
        next-card       (first -deck)
        deck            (into [] (rest -deck))
        position        (if (= :epilogue-open (:type next-card))
                          (roll-theory (count (->> (map :type scenes) (keep #{:clue}))))
                          (cond
                            (:de-escalate active-tags)
                            (max 1 (dec position))
                            (:escalate active-tags)
                            (min 5 (inc position))
                            :else
                            position))
        [next-card
         prompt-decks]  (draw-prompt next-card
                                     (get position-tags position [:green])
                                     -prompt-decks)
        stage-info      (get-stage-info game next-card)
        next-next       (next-player player-order next-up)
        max-clock       8
        clocks          (if (and (> max-clock (-> clocks :plot)) (:advance active-tags))
                          (update clocks :plot inc)
                          clocks)
        next-clock      (-> clocks :plot)

        next-game          (assoc game
                                  :-deck deck
                                  :clocks clocks
                                  :position (cond
                                              ;; Don't ratchet position in end-game
                                              (#{:epilogue-open :epilogue :epilogue-close} (:type next-card))
                                              position
                                              (> next-clock 7)
                                              (max 4 position)
                                              (> next-clock 5)
                                              (max 3 position)
                                              (> next-clock 3)
                                              (max 2 position)
                                              :else
                                              position)
                                  :-prompt-decks prompt-decks
                                  :dossiers dossiers
                                  :scenes (if (and (:scene next-card)
                                                   (not ((into #{} scenes) (:scene next-card))))
                                            (concat scenes [(:scene next-card)])
                                            scenes)
                                  :-discard discard
                                  :-last-state game
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
                -prompt-decks
                position-tags
                position
                -discard
                -deck
                stage]} game
        active-card     (get-in game [:active-display :card])
        next-up         (next-player player-order active-player)
        discard         (cons active-card -discard)
        next-card       (first -deck)
        deck            (rest -deck)
        [next-card
         prompt-decks]  (draw-prompt next-card
                                     (get position-tags position [:green])
                                     -prompt-decks)
        stage-info      (get-stage-info game next-card)

        next-game          (-> game
                            (assoc-in [:inactive-display :x-card-active?] false)
                            (assoc :-deck deck
                                   :-last-state game
                                   :-prompt-decks prompt-decks
                                   :-discard discard))
        new-active-display (build-active-card next-game next-card active-player next-up)]
    (assoc (merge next-game stage-info)
           :active-display new-active-display
           :inactive-display (build-inactive-version next-game new-active-display))))

(defn jump-ahead [game]
  (let [{:keys [player-order
                active-player
                -prompt-decks
                position-tags
                position
                -discard
                -deck
                stage]} game
        active-card     (get-in game [:active-display :card])
        next-up         (next-player player-order active-player)
        discard         (cons active-card -discard)
        [discard -deck] (loop [x discard
                               d -deck]
                          (if (or (-> d first :type #{:theory})
                                  (empty? d))
                            [x d]
                            (recur (cons (first d) x) (rest d))))
        next-card       (first -deck)
        _ (println next-card)
        deck            (rest -deck)
        [next-card
         prompt-decks]  (draw-prompt next-card
                                     (get position-tags position [:green])
                                     -prompt-decks)
        stage-info      (get-stage-info game next-card)
        next-game       (-> game
                            (assoc-in [:inactive-display :x-card-active?] false)
                            (assoc :-deck deck
                                   :-last-state game
                                   :-prompt-decks prompt-decks
                                   :-discard discard))

        new-active-display (build-active-card next-game next-card active-player next-up)]
    (assoc (merge next-game stage-info)
           :active-display new-active-display
           :inactive-display (build-inactive-version next-game new-active-display))))

(defn pass-card [game]
  (let [{:keys [player-order
                active-player]} game
        active-card             (get-in game [:active-display :card])
        next-up                 (next-player player-order active-player)
        next-next               (next-player player-order next-up)
        next-game               (assoc game :active-player next-up :-last-state game)
        new-active-display      (build-active-card next-game active-card next-up next-next)]
    (assoc next-game
           :inactive-display (build-inactive-version next-game new-active-display)
           :active-display new-active-display)))

;; TODO: How much stress does this add to duratom?
(defn undo-card [game]
  (let [{:keys [player-scores
                -last-state]} game]
    (assoc -last-state
           :player-scores
           (get-in game [:player-scores]))))

(defn push-uniq [coll item]
  (if (some #(= % item) coll)
    coll
    (into [item] coll)))

(defn upstress-player
  [voter-id
   {:keys [player-id]}
   {:keys [all-players dossiers] :as game}]
  (let [current-score (get-in dossiers [player-id :stress])
        new-score (min (inc current-score) 3)
        player-names (group-by :id all-players)
        player-name (or (get-in dossiers [player-id :agent-codename])
                        (get-in player-names [player-id 0 :user-name] "Someone"))]
    (if current-score
      (-> game
          (assoc-in [:dossiers player-id :stress] new-score)
          (assoc-in [:broadcasts] [[:->toast/show! (str "😬 " player-name " took stress.")]]))
      game)))

(defn downstress-player
  [voter-id
   {:keys [player-id]}
   {:keys [all-players dossiers] :as game}]
  (let [current-score (get-in dossiers [player-id :stress])
        new-score (max (dec current-score) 0)
        player-names (group-by :id all-players)
        player-name (or (get-in dossiers [player-id :agent-codename])
                        (get-in player-names [player-id 0 :user-name] "Someone"))]
    (if current-score
      (-> game
          (assoc-in [:dossiers player-id :stress] new-score)
          (assoc-in [:broadcasts] [[:->toast/show! (str "😅 " player-name " removed stress.")]]))
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

(defn regen-card [params
                  {:keys [-generators dossier-template active-player player-order stage]
                   :as   game}]
  (let [next-up      (next-player player-order active-player)
        next-dossier (build-active-card game
                                        (dossier-card dossier-template -generators active-player params)
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

(defn take-action [{:keys [uid room-id action params]} {:keys [game]}]
  (let [do-next-state (case action
                        :done            finish-card
                        :x-card          x-card
                        :discard         discard-card
                        :pass            pass-card
                        :undo            undo-card
                        :regen           (partial regen-card params)
                        ;; TODO - work out upvote/downvote UI for players
                        :upstress-player   (partial upstress-player uid params)
                        :downstress-player (partial downstress-player uid params)
                        :tick-clock      tick-clock
                        ;; TODO allow players to leave game without ending
                         ;;; change action text
                        :jump-ahead      jump-ahead
                        :leave-game      end-game
                        :end-game        end-game)
        execute       (if-active-> uid action do-next-state)]
    (try
      (execute game)
      (catch Exception e (println e)))))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
