(ns tenkiwi.util
  )

(defmacro inspect
  [expression]
  (list 'let ['result expression]
        (list 'println (list 'quote expression) "=>" 'result)
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
         rows   (map #(zipmap keys %) rest)]
     (map-indexed parser rows))))
