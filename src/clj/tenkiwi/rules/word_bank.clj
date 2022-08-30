(ns tenkiwi.rules.word-bank
  )

(def $ :-word-bank)

(defn- extract-generator-list [str]
  (->> (clojure.string/split str #"\s\s")
       (map #(clojure.string/split % #":\s+"))
       (map #(hash-map :name (first %)
                       :label (last %)))))

(defn- gather-word-banks [-generators story-details word-count]
  (map #(hash-map :title (:label %)
                  :name (:name %)
                  :items (take (:count % word-count) (shuffle (mapv :text (get -generators (:name %) [])))))
       story-details))

(defn initial-state [starting-state
                     {:keys [word-banks word-bank-key generators]
                      :or   {word-bank-key :story-details}
                      :as   options}]
  (let [word-banks  (if (string? word-banks)
                      (extract-generator-list word-banks)
                      word-banks)
        extra-state {:word-banks        word-banks
                     :word-bank-key     word-bank-key
                     :word-bank-count   3
                     :generators        generators
                     :current-word-bank (gather-word-banks generators word-banks 3)}]
    (merge
     starting-state
     {$ extra-state})))

(defn ->word-banks [game]
  (get-in game [$ :current-word-bank]))

(defn regen-word-banks!
  ([game]
   (let [word-banks      (get-in game [$ :word-banks])
         word-bank-count (get-in game [$ :word-bank-count])]
     (regen-word-banks! game word-banks word-bank-count)))
  ([{:as                  game
     {:keys [generators]} $}
    word-banks
    word-bank-count]
   (assoc-in game
             [$ :current-word-bank]
             (gather-word-banks generators word-banks word-bank-count))))

(defn render-display [game]
  (let [rendered (->word-banks game)
        key      (get-in game [$ :word-bank-key] :story-details)]
    (assoc-in game [:display key] rendered)))

;; TODO - use util/pluck-text here?
(defn ->pluck
  ([game gen-name]
   (first (->pluck game gen-name 1)))
  ([{:as game
     {:keys [generators]} $}
    gen-name
    count]
   (->> [{:text "Foo"}]
        (get generators gen-name)
        shuffle
        (take count)
        (map :text))))
