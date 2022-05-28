(ns tenkiwi.rules.prompt-deck
  (:require [tenkiwi.rules.player-order :as player-order]))

(def STATE-KEY :-prompt-deck)

(defn initial-state [starting-state
                     {:keys [deck discard features fillers]
                      :or   {discard [] features {} fillers {}}
                      :as   options}]
  (let [order        (into [] deck)
        discard      (into [] discard)
        order-state  {:deck        (rest order)
                      :fillers     fillers
                      :features    features
                      :discard     discard
                      :active-card (first order)}]
    (merge
     starting-state
     {STATE-KEY order-state})))

(defn active-card [{{:keys [flags
                            active-card]} STATE-KEY
                    :as                   game}]
  (merge active-card flags))

(defn render-display [state]
  (let [active-card (active-card state)]
    (-> state
        (update :display assoc :card active-card))))

(defn flag-card! [game new-flags]
  (update-in game [STATE-KEY :flags] merge new-flags))

(defn card-passed! [game] (flag-card! game {:passed? true}))

(defn build-feature-flags [game flags]
  (cond-> {}
    (:everyone flags)
    (assoc :starting-player (player-order/active-player game))))

(defn ->everyone? [game]
  (let [up-next (player-order/next-player game)
        tags    (get-in game [STATE-KEY :active-card :tags])
        starter (get-in game [STATE-KEY :flags :starting-player])
        passed? (get-in game [STATE-KEY :flags :passed?])]
    (or (not (:everyone tags))
        (and passed? (= starter up-next)))))

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

(defn- safe-inc [val]
  (if-not (number? val)
    1
    (inc val)))

(defn -draw-filler [filler-card fillers subset]
  (let [matcher (fn filler-match? [b]
                  (and
                   (every? (:tags b) (:required-tags filler-card))
                   (some (:tags b) (:tags subset))
                   (= (select-keys filler-card (keys (dissoc subset :tags)))
                      (select-keys b (keys (dissoc subset :tags))))))
        deck-type             (:type filler-card)
        prompts               (get fillers deck-type [])
        [matches non-matches] ((juxt filter remove)
                               matcher
                               ;; sorting after shuffle allows us to
                               ;; semi-randomize cards, reordering so less drawn
                               ;; cards always in front, but all cards with same
                               ;; number are shuffled (because sort-by is a
                               ;; stable sort)
                               (sort-by #(:draws % 0) (shuffle prompts)))
        next-card             (merge filler-card (first matches))
        ;; This increments a "draw" counter on the card matched
        fillers               (update fillers deck-type (partial map (fn [prompt]
                                                                       (if (= (first matches) prompt)
                                                                         (update prompt :draws safe-inc)
                                                                         prompt))))]
    ;; Note this will return even if no matching prompt (giving an unfilled card)
    [next-card fillers]))

(defn -replace-filler [filler-card fillers subset]
  (cond
    (and (:filler? filler-card) (get fillers (:type filler-card)))
    (-draw-filler filler-card fillers subset)
    :else
    [filler-card fillers]))

(defn fill-card! [{{:keys [fillers active-card
                           features]} STATE-KEY
                   :as                game}
                  matcher]
  (let [replace-filler?               (:replace-filler? features)
        [new-active-card new-fillers] (-replace-filler active-card fillers matcher)
        ;; Leave fillers unaltered if "replace-filler?" option used
        new-fillers                   (if replace-filler? fillers new-fillers)]
    (-> game
       (assoc-in [STATE-KEY :active-card] new-active-card)
       (assoc-in [STATE-KEY :fillers] new-fillers))))

(defn draw-next-card! [{:as                                game
                        {:keys [features fillers deck
                                discard active-card]} STATE-KEY}]
  (let [new-flags   (build-feature-flags game features)
        new-active  (first deck)
        new-discard (cons active-card discard)
        new-deck    (into [] (rest deck))]
    (assoc game
           STATE-KEY {:deck        new-deck
                      :fillers     fillers
                      :flags       new-flags
                      :features    features
                      :discard     new-discard
                      :active-card new-active})))
