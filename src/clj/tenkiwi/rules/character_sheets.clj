(ns tenkiwi.rules.character-sheets
  (:require [tenkiwi.util :as util]
            [tenkiwi.rules.word-bank :as word-bank]
            [tenkiwi.rules.prompt-deck :as prompt-deck]
            [tenkiwi.rules.player-order :as player-order]))

(def $ :-character-sheets)

(defn- build-inputs [state fields current-values]
  (let [pluck-value (fn [keyname]
                      (or (get current-values keyname)
                          (word-bank/->pluck state (name keyname))))]
    (mapv #(hash-map :name (first %)
                     :label (last %)
                     :value (pluck-value (first %)))
          fields)))

(defn- pluck-fields [state fields current-values]
  (let [pluck-value (fn [keyname]
                      (or (get current-values keyname)
                          (word-bank/->pluck state (name keyname))))]
    (zipmap (keys fields)
            (map pluck-value (keys fields)))))

(defn initial-state [starting-state
                     {:keys [name-key intro-card players]
                      :or   {name-key :nickname}
                      :as   options}]
  (let [generator-list (->> (clojure.string/split (:inputs intro-card) #"\s\s")
                            (map #(clojure.string/split % #":"))
                            (into {}))

        fields      (util/update-keys generator-list keyword)
        sheet-state {:name-key   name-key
                     :fields     fields
                     :intro-card intro-card
                     :sheets     (zipmap (map :id players)
                                         (map #(merge
                                                (pluck-fields starting-state fields {})
                                                (select-keys % [:id :user-name]))
                                              players))}]
    (assoc starting-state $ sheet-state)))

(defn ->intro-card
  ([state player]
   (->intro-card state player (get-in state [$ :sheets (:id player)])))
  ([state player current-values]
   (let [fields      (get-in state [$ :fields])
         intro-card  (get-in state [$ :intro-card])]
     (merge intro-card
            {:id     :player-dossier
             :dossier-placeholder? true
             :inputs (build-inputs state fields current-values)}))))

(defn ->player-names
  ([state]
   (->player-names state :user-name))
  ([state fallback-key]
   (let [sheets      (get-in state [$ :sheets])
         name-key    (get-in state [$ :name-key])]
     (util/update-values sheets
                         (fn [%] (if (:locked? %)
                                   (get % name-key (get % fallback-key "Someone"))
                                   (get % fallback-key "Someone")))))))

(defn placeholder [intro-card player]
  (merge intro-card
         {:id :player-dossier
          :dossier-placeholder? true}))

(defn set-sheet! [state {:keys [id]} vals]
  (assoc-in state
            [$ :sheets id]
            vals))

(defn regen! [state player vals]
  (let [fields (get-in state [$ :fields])]
    (set-sheet! state player (pluck-fields state fields vals))))

(defn lock-sheet! [state player]
  (assoc-in state
            [$ :sheets (:id player)]
            :locked?
            true))

(defn maybe-lock-sheet! [state]
  (let [active-player (player-order/active-player state)
        card         (prompt-deck/active-card state)]

    (if (:dossier-placeholder? card)
      (assoc-in state [$ :sheets (:id active-player) :locked?] true)
      state)))

(defn render-display [state]
  (let [sheets        (get-in state [$ :sheets])
        fields        (get-in state [$ :fields])
        name-key      (get-in state [$ :name-key])
        current       (player-order/active-player state)
        current-sheet (if (get-in sheets [(:id current) :locked?])
                        (get sheets (:id current) {})
                        {})
        card          (prompt-deck/active-card state)]
    (cond-> state
      true
      (assoc-in [:display :sheets] (util/keep-values sheets :locked?))
      true
      (assoc-in [:display :turn-marker]
                (str (if (name-key current-sheet)
                       (str (name-key current-sheet)
                            " (" (:user-name current) ")")
                       (:user-name current))
                     "'s turn..."))
      (:dossier-placeholder? card)
      (update-in [:display :card]
                 merge
                 (->intro-card state current))
      )))
