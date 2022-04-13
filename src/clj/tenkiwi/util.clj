(ns tenkiwi.util
  )

(defmacro inspect
  [expression]
  (list 'let ['result expression]
        (list 'pr (list 'quote expression) "=>" 'result)
        (list 'print "\n")
        'result))

(defn remove-values [m f & args]
  (reduce (fn [r [k v]] (if (apply f v args)
                          r
                          (assoc r k v))) {} m))

(defn keep-values [m f & args]
  (reduce (fn [r [k v]] (if (apply f v args)
                          (assoc r k v)
                          r)) {} m))

(defn update-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn update-keys [m f & args]
  (reduce (fn [r [k v]] (assoc r (apply f k args) v)) {} m))

(def google-sheet-match
  #"https://docs.google.com/spreadsheets/d/e/(.+)/pub(?:html)?\??(gid=([^&]+)&.+)?")

(defn- extract-google-tsv [url]
  (if-let [match (re-matches google-sheet-match
                             (clojure.string/trim url))]
    (let [[_ id _ gid] match
          gid (or gid "0")]
      (str "https://docs.google.com/spreadsheets/d/e/" id "/pub?gid=" gid "&single=true&output=tsv"))
    url))


;; TODO: Make this a bit more error proof and check what happens in failures
(defn pull-tsv [url]
  (-> url
      clojure.string/trim
      extract-google-tsv
      slurp))

(defn read-spreadsheet-data
  ([url]
   (read-spreadsheet-data url second))
  ([url parser]
   (let [row-filter (fn [r] (->> r
                                 (filter :text)
                                 (filter :type)))]
     (read-spreadsheet-data url parser row-filter)))
  ([url parser row-filter]
   (let [lines
         (->> (pull-tsv url)
              (clojure.string/split-lines)
              (take 1000)
              (map #(clojure.string/split % #"\t")))
         header (first lines)
         rest   (rest lines)
         keys   (map keyword header)
         rows   (->> (map #(zipmap keys %) rest)
                     row-filter)]
     (map-indexed parser rows))))

(defn normalize-twospace [text]
  (clojure.string/replace text #"\s\s" "\n\n"))


(defn extract-tags [{:as card
                     :keys [text]}]
  (let [[tag-line rest] (if (clojure.string/starts-with? text "#")
                          (clojure.string/split text #"\n\n" 2)
                          ["" text])
        tags
        (if (empty? tag-line)
          {}
          (reduce #(assoc %1 (clojure.string/replace %2 "#" "") true)
                  {}
                  (clojure.string/split tag-line #"(,|\s+)")))
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
  (let [cards    (read-spreadsheet-data url normalize-card)
        includes (->> (group-by :type cards)
                      :include
                      (take 50)
                      (map :text))
        cards    (concat cards
                         (mapcat #(read-spreadsheet-data % normalize-card)
                                 includes))]
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

(defn roll [count sides]
  (map (fn [i] (inc (rand-int sides))) (range 0 count)))

(defn push-uniq [coll item]
  (if (some #(= % item) coll)
    coll
    (into [item] coll)))
