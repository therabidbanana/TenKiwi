(ns tenkiwi.rules.oracle-box
  (:require
   [tenkiwi.util :as util :refer [inspect]]
   [tenkiwi.lib.dice :as dice]
   ;; [tenkiwi.rules.character-sheets :as character-sheets]
   )
  )

(def $ :-oracle-box)

(defn initial-state [starting-state
                     {:keys [id title description
                             table log limit push?]
                      :or   {log   []
                             limit 10
                             title ""
                             table {}
                             push? false}
                      :as   options}]
  (let [extra-state {:features    {:push? push?}
                     :table       table
                     :push?       push?
                     :id          id
                     :title       title
                     :description description
                     :log         log
                     :limit       limit}]
    (assoc-in starting-state [$ id] extra-state)))

(defn draw-push [{:keys [push? last id
                         limit log table]
                  :as   oracle}
                 reroll?]
  (let [modifier                (if reroll?
                                  (get last :result 0)
                                  0)
        {:as   roll
         :keys [::dice/text
                ::dice/roll
                ::dice/result]} (first (dice/parse-and-roll (str "1d6+" modifier)))
        push                    (cond
                                  (> result 6) "miss"
                                  (> result 4) "strong"
                                  :else        "weak")]
    {:text   (-> (get-in table [push])
                 rand-nth
                 :text)
     :label  (cond
               (= :oracle id) ({"miss" "Misfortune" "strong" "Unlikely" "weak" "Likely"} push)
               :else ({"miss" "Miss" "strong" "Strong Hit" "weak" "Weak Hit"} push)
               )
     :result result}))

(defn draw-from [{:keys [push? last
                         limit log table]
                  :as   oracle}
                 reroll?]
  (let [list (if (map? table)
                (vals table)
                list)
        random-sample (cond
                        (empty? table)
                        {:text "Nothing"}
                        reroll?
                        (rand-nth (remove #(= (:number %)
                                              (:result last))
                                          list))
                        :else
                        (rand-nth list))]
    (cond push?
          (draw-push oracle reroll?)
          :else
          {:text   (:text random-sample "")
           :result (:number random-sample "")})))

(defn consult! [game
             {:keys [id reroll? replace-vars]
              :as   params}]
  (let [{:keys [push? last
                limit log table]
         :as   oracle} (get-in game [$ id] {})

        result  (draw-from oracle reroll?)
        result  (if replace-vars
                  (replace-vars game result)
                  result)
        log     (if reroll?
                  (rest log)
                  log)
        new-log (if limit
                  (take limit (cons result log))
                  (cons result log))]
    (-> game
        (assoc-in [$ id :log] new-log)
        (assoc-in [$ id :last] result))))

(defn consult-action [{:keys [text id reroll?]}]
  {:text   text
   :action :consult
   :params {:id id
            :reroll? reroll?}})

(defn render-oracle-display [state]
  (let [oracles (get-in state [$])]
    (-> state
        (update :display assoc :oracle-box
                (util/update-values
                 oracles
                 (fn [{:keys [last log id title description] :as foo}]
                   {:current     last
                    :log         log
                    :title       title
                    :description description
                    :actions     (keep identity
                                       [(consult-action {:id id :text "Consult" :reroll? false})
                                        (if (-> last :result (or 9) (< 5))
                                          (consult-action {:id id :text "Push" :reroll? true}))])}))))))
