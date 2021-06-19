(ns tenkiwi.util
  )

(defmacro inspect
  [expression]
  (list 'let ['result expression]
        (list 'pr (list 'quote expression) "=>" 'result)
        (list 'print "\n")
        'result))

(defn update-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn update-keys [m f & args]
  (reduce (fn [r [k v]] (assoc r (apply f k args) v)) {} m))

(defn read-spreadsheet-data
  ([url]
   (read-spreadsheet-data url second))
  ([url parser]
   (let [lines
         (->> (slurp url)
              (clojure.string/split-lines)
              (map #(clojure.string/split % #"\t")))
         header (first lines)
         rest   (rest lines)
         keys   (map keyword header)
         rows   (->> (map #(zipmap keys %) rest)
                     (filter :text)
                     (filter :type))]
     (map-indexed parser rows))))

(defn normalize-twospace [text]
  (clojure.string/replace text #"\s\s" "\n\n"))


(defn extract-tags [{:as card
                     :keys [text]}]
  (let [[tag-line rest] (if (clojure.string/starts-with? text "#")
                          (clojure.string/split text #"\n\n")
                          ["" text])
        tags
        (reduce #(assoc %1 (clojure.string/replace %2 "#" "") true)
                {}
                (clojure.string/split tag-line #","))
        ]
    (assoc card
           :tags (update-keys tags keyword)
           :text rest)))

(defn normalize-card [id map]
  (-> (assoc map :id id)
      (update :type keyword)
      (update :text normalize-twospace)
      (extract-tags)
      ))

(defn gather-decks [url]
  (let [cards (read-spreadsheet-data url normalize-card)]
    (group-by :type cards)))

(defn index-by [fn coll]
  (let [grouped (group-by fn coll)]
    (zipmap (keys grouped)
            (map first (vals grouped)))))

(defn pluck-text
  ([generators keyname]
   (first (pluck-text generators keyname 1)))
  ([generators keyname n]
   (->> (get generators keyname [{:text "unknown"}])
        shuffle
        (map :text)
        (take n))))
