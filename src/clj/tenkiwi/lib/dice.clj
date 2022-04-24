(ns tenkiwi.lib.dice
  (:require
   [instaparse.core :as instaparse]
   [clojure.edn :as edn]
   )
  )

(def parser
  (instaparse/parser
   "
<sentence> = (formula / <';'> / <space> / raw-text)+
formula = dice (modifier)? (<space> descriptive-text)?
modifier = #'([+-])?\\d+'
operator = '+' | '-'
descriptive-text = (raw-text)
dice = (count)? <('d' | 'D')> sides
count = number
sides = number
number = #'\\d+'
raw-text = (<word> | <space>)+
word = #'[^\\s;]+'
space = #'\\s'
"))

(defn- take-formula [text formula]
  (let [span         (instaparse/span formula)
        [tag & opts] formula
        raw-text     (apply subs text span)]
    (if (= :formula tag)
      (into {:text raw-text} opts))))

(defn parse-formula [text]
  (let [tokens      (parser (or text ""))
        transformed (instaparse/transform {:number   edn/read-string
                                           :count    #(hash-map :count %)
                                           :sides    #(hash-map :sides %)
                                           :dice     (fn [& opts] (into {:count 1 :modifier 0} opts))
                                           :modifier #(hash-map :modifier (edn/read-string %))}
                                          tokens)
        formulas    (keep (partial take-formula text) transformed)]
    formulas))

(defn roll-it [{:as   formula
                :keys [modifier count sides text]
                :or   {modifier 0
                       count    1}}]
  (let [rolls      (take count (repeatedly #(inc (rand-int sides))))
        dice-total (reduce + rolls)]
    {::parsed     formula
     ::text       text
     ::modifier   (if (< 0 modifier)
                    (str " +" modifier)
                    (str " " modifier))
     ::roll       (into [] rolls)
     ::dice-total dice-total
     ::result     (+ modifier dice-total)}))

(defn roll-formula [formulas]
  (mapv roll-it formulas))

(defn parse-and-roll [text]
  (-> text
      parse-formula
      roll-formula))
