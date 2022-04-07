(ns tenkiwi.rules.word-bank
  )

(def $ :-word-bank)

(defn- extract-generator-list [str]
  (->> (clojure.string/split str #"\s\s")
       (map #(clojure.string/split % #":\s+"))
       (map #(hash-map :name (first %)
                       :label (last %)))))

(defn initial-state [starting-state
                     {:keys [word-banks word-bank-key generators]
                      :or   {word-bank-key :story-details}
                      :as   options}]
  (let [word-bank   (if (string? word-banks)
                      (extract-generator-list word-banks)
                      word-banks)
        extra-state {:word-banks      word-bank
                     :word-bank-key   word-bank-key
                     :word-bank-count 3
                     :generators      generators}]
    (merge
     starting-state
     {$ extra-state})))

(defn- gather-word-banks [-generators story-details word-count]
  (map #(hash-map :title (:label %)
                 :name (:name %)
                 :items (take 3 (shuffle (mapv :text (get -generators (:name %) [])))))
      story-details))

(defn ->word-banks
  ([game]
   (let [word-banks (get-in game [$ :word-banks])
         word-bank-count (get-in game [$ :word-banks])]
     (->word-banks game word-banks word-bank-count)))
  ([{{:keys [generators]} $} word-banks word-bank-count]
   (gather-word-banks generators word-banks word-bank-count)))

(defn ->pluck
  ([{{:keys [generators]} $} gen-name]
   (first (->pluck generators gen-name 1)))
  ([{{:keys [generators]} $} gen-name count]
   (->> [{:text "Foo"}]
        (get generators gen-name)
        shuffle
        (take count)
        (map :text))))
