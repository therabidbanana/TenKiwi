(ns tenkiwi.util
  )

(defmacro inspect
  [expression]
  (list 'let ['result expression]
        (list 'pr (list 'quote expression) "=>" 'result)
        (list 'print "\n")
        'result))

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

(defn normalize-card [id map]
  (-> (assoc map :id id)
      (update :type keyword)
      (update :text normalize-twospace)))

(defn gather-decks [url]
  (let [cards (read-spreadsheet-data url normalize-card)]
    (group-by :type cards)))

(defn index-by [fn coll]
  (let [grouped (group-by fn coll)]
    (zipmap (keys grouped)
            (map first (vals grouped)))))

(defn pluck-text [generators keyname]
  (-> generators
      (get keyname [{:text "unknown"}])
      shuffle
      first
      :text))
