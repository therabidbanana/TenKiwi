(ns tenkiwi.gamemasters.wretched
  "This is the game logic for Wretched and Alone games"
  (:require
   [clojure.string :as string]
   [tenkiwi.tables.debrief :as tables]
   [tenkiwi.util :as util :refer [inspect]]))

(def valid-active-actions #{:collapsed :pass :discard :undo :done :x-card :end-game :next-image :previous-image :leave-game})
(def valid-inactive-actions #{:x-card :undo :leave-game})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))

(def collapsed-action
  {:action :collapsed
   :text   "Tower Collapsed"})

(def next-image-action
  {:action :next-image
   :class  :next-button
   :text   ">"})

(def previous-image-action
  {:action :previous-image
   :class  :previous-button
   :text   "<"})

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

(def undo-action
  {:action  :undo
   :text    "Undo Last"})

(defn extract-vars [{:keys [active-player next-players
                            dossiers mission]
                     :as   game}
                    card]
  (let [next-player   (:agent-id (first next-players))
        prev-player   (:agent-id (last next-players))]
    {:player-left    (get-in game [:dossiers prev-player :codename] "")
     :player-right   (get-in game [:dossiers next-player :codename] "")}))

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

         pass                  (if next-player
                                 {:action :pass
                                  :text   (str "Pass card to " (:user-name next-player))})]
     {:card              (assoc (replace-vars game card)
                                :starting-player starting-player)
      :extra-details     (concat
                          scene-details
                          (map #(hash-map :title (:label %)
                                          :name (:name %)
                                          :items (take 3 (shuffle (mapv :text (get -generators (:name %) [])))))
                               story-details))
      :extra-actions     [undo-action next-image-action previous-image-action leave-game-action]
      :available-actions valid-active-actions
      :actions           (keep identity
                               (case next-stage
                                 :ending-doom [pass end-game-action]
                                 :ending-early [pass end-game-action]
                                 :ending-success [pass end-game-action]
                                 :ending-fail [pass end-game-action]
                                 (if can-finish?
                                   [done-action pass]
                                   [pass])))}))
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


(defn one-per-group
  ([act-collection]
   (let [grouped (group-by :group act-collection)]
     (zipmap (keys grouped)
             (map first (vals grouped)))))
  ([act-collection func]
   (let [grouped (group-by :group act-collection)]
     (zipmap (keys grouped)
             (map #(-> % first func) (vals grouped))))))

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

(defn select-game [room-id {:keys [game-url extra-players mission-id]
                           :as   params
                           :or   {}}
                  {:keys [players] :as room}]
  (let [first-player        (first players)
        decks               (util/gather-decks game-url)

        new-game       {:game-type        :wretched
                        :configuration    {:params params
                                           :inputs []}}]
    new-game))

(defn start-game [room-id {:keys [game-url mission-id extra-players]
                           :as   params
                           :or   {extra-players 0}}
                  {:keys [players] :as room}]
  (let [original-players  players
        players           (map-indexed
                           #(assoc %2 :agent-id %1)
                           (take (+ extra-players (count original-players))
                                 (cycle original-players)))
        first-player      (first players)
        next-players      (rest players)
        player-count      (count players)
        next-player       (first next-players)

        {intro-cards :intro
         images      :image
         prompts     :prompt
         climax      :climax
         good-ending :ending-success
         bad-ending  :ending-fail
         doom-ending :ending-doom
         collapse    :ending-early
         :as         decks} (util/gather-decks game-url)

        tower (util/roll 100 6)

        active-display (build-active-card (first intro-cards) first-player next-player)
        new-game       {:player-order     (into [] original-players)
                        :all-players      players
                        :active-player    (first players)
                        :next-players     (rest players)
                        :game-type        :wretched
                        :stage-names      {:intro            "Introduction"
                                           :epilogue-open       "Resolution"
                                           :epilogue            "Resolution"
                                           :epilogue-close      "Resolution"}
                        :-tower           tower
                        :clocks           {:tower (count (filter #{1} tower))
                                           :clues 0 :doom 0}
                        :stage            :intro
                        :stage-name       "Introduction"
                        :scenes           []
                        :image-deck       (into [] (rest images))
                        :image            (first images)
                        :-discard         []
                        :-endings         {:climax (first climax)
                                           :good (first good-ending)
                                           :bad  (first bad-ending)
                                           :doom (first doom-ending)
                                           :collapse (first collapse)}
                        :-deck            (into []
                                                (concat (rest intro-cards)
                                                        (shuffle prompts)))
                        :active-display   active-display
                        :inactive-display (build-inactive-version {:active-player (first players)} active-display)}]
    new-game))

(comment
  (let [game (start-game "foo" {:extra-players 1} {:players [{:name "David"}]})]
    (:-deck game))

  )

(defn extract-dossier [{:keys [inputs]}]
  (-> (zipmap (map keyword (map :name inputs))
              (map :value inputs))
      (assoc :stress 0)))

(defn get-stage-info [{:keys [stage-names]
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
        next-card             (merge next-card (first matches))
        ]
    (if-not (empty? prompts)
      [next-card (assoc prompt-decks deck-type (concat (rest matches) non-matches))]
      [next-card prompt-decks]
      )))


(defn pull-from-tower [tags tower clocks]
  (let [[ones others] ((juxt filter remove)
                       #{1}
                       tower)
        next-tower    (concat ones (util/roll (count others) 6))
        next-clocks   (assoc clocks :tower (count (filter #{1} next-tower)))]
    (cond
      (:pull tags)
      [next-tower next-clocks]
      (:double-pull tags)
      (pull-from-tower {:pull true} next-tower next-clocks)
      :else
      [tower clocks])))

(defn finish-card [game]
  (let [{:keys [active-player
                next-players
                scenes
                -endings
                -tower
                clocks
                -discard
                -deck
                stage]} game
        active-card     (get-in game [:active-display :card])
        active-tags     (:tags active-card)
        active-type     (:type active-card)
        all-players     (conj (into [] next-players) active-player)
        next-up         (first all-players)
        next-players    (rest all-players)
        discard         (cons active-card -discard)
        next-card       (first -deck)
        deck            (into [] (rest -deck))

        next-next       (first next-players)
        [-tower clocks] (pull-from-tower active-tags -tower clocks)
        clocks          (cond-> clocks
                          (:doom active-tags) (update :doom inc)
                          (:clue active-tags) (update :clues inc))
        end-state       (cond
                          (and (> (:clues clocks) 3)
                               (> (:tower clocks) 99)) :bad
                          (and (> (:clues clocks 3))
                               (#{:climax} active-type)) :good
                          (> (:clues clocks) 3) :climax
                          (> (:tower clocks) 99) :collapse
                          (> (:doom clocks) 3) :doom)
        next-card       (if end-state
                          (get -endings end-state next-card)
                          next-card)

        next-game          (assoc game
                                  :-deck deck
                                  :-tower -tower
                                  :clocks (inspect clocks)
                                  :-discard discard
                                  :-last-state game
                                  :active-player next-up
                                  :next-players next-players)
        new-active-display (build-active-card next-game next-card next-up next-next)]
    (-> next-game
        (assoc
         :active-display new-active-display
         :inactive-display (build-inactive-version next-game new-active-display)))))

(defn discard-card [game]
  (let [{:keys [active-player
                next-players
                -prompt-decks
                position-tags
                position
                -discard
                -deck
                stage]} game
        active-card     (get-in game [:active-display :card])
        next-up         (first next-players)
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

(defn pass-card [game]
  (let [{:keys [active-player
                next-players]} game
        active-card            (get-in game [:active-display :card])
        all-players            (conj (into [] next-players) active-player)
        next-up                (first all-players)
        next-players           (rest all-players)
        next-next              (first next-players)
        next-game              (assoc game
                                      :next-players next-players
                                      :active-player next-up
                                      :-last-state game)
        new-active-display     (build-active-card next-game active-card next-up next-next)]
    (assoc next-game
           :inactive-display (build-inactive-version next-game new-active-display)
           :active-display new-active-display)))

;; TODO: How much stress does this add to duratom?
(defn undo-card [game]
  (let [{:keys [-last-state]} game]
    -last-state))

(defn previous-image [game]
  (let [{:keys [image-deck
                image]} game
        new-image      (last image-deck)
        new-image-deck (into [image] (pop image-deck))]
    (assoc game
           :image new-image
           :image-deck new-image-deck)))

(defn next-image [game]
  (let [{:keys [image-deck
                image]} game
        next-image      (first image-deck)
        next-image-deck (conj (into [] (rest image-deck)) image)]
    (assoc game
           :image next-image
           :image-deck next-image-deck)))

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
  ;; Nothing
  game)

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
                        :next-image      next-image
                        :previous-image  previous-image
                        :tick-clock      tick-clock
                        ;; TODO allow players to leave game without ending
                         ;;; change action text
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
