(ns tenkiwi.rules.word-bank
  )

(def $ :-word-bank)

(defn- extract-generator-list [str]
  (->> (clojure.string/split str #"\s\s")
       (map #(clojure.string/split % #":\s+"))
       (map #(hash-map :name (first %)
                       :label (last %)))))

(defn initial-state [starting-state
                     {:keys [word-banks word-bank-path generators]
                      :or   {word-bank-path [:active-display :story-details]}
                      :as   options}]
  (let [word-banks  (if (string? word-banks)
                      (extract-generator-list word-banks)
                      word-banks)
        extra-state {:word-banks      word-banks
                     :word-bank-path  word-bank-path
                     :word-bank-count 3
                     :generators      generators}]
    (merge
     starting-state
     {$ extra-state})))

(defn- gather-word-banks [-generators story-details word-count]
  (map #(hash-map :title (:label %)
                 :name (:name %)
                 :items (take word-count (shuffle (mapv :text (get -generators (:name %) [])))))
      story-details))

(defn ->word-banks
  ([game]
   (let [word-banks (get-in game [$ :word-banks])
         word-bank-count (get-in game [$ :word-bank-count])]
     (->word-banks game word-banks word-bank-count)))
  ([{{:keys [generators]} $} word-banks word-bank-count]
   (gather-word-banks generators word-banks word-bank-count)))

(defn render-word-banks!
  ([game]
   (render-word-banks! game (get-in game [$ :word-bank-path])))
  ([game path]
   (let [rendered (->word-banks game)]
     (assoc-in game path rendered))))

(defn ->pluck
  ([{{:keys [generators]} $} gen-name]
   (first (->pluck generators gen-name 1)))
  ([{{:keys [generators]} $} gen-name count]
   (->> [{:text "Foo"}]
        (get generators gen-name)
        shuffle
        (take count)
        (map :text))))
