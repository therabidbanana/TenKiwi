(ns tenkiwi.rules.dice-bag
  (:require
   [tenkiwi.util :as util :refer [inspect]]
   [tenkiwi.lib.dice :as dice]
   ;; [tenkiwi.rules.character-sheets :as character-sheets]
   )
  )

(def $ :-dice-bag)

(defn initial-state [starting-state
                     {:keys [shortcuts input? log limit charge?]
                      :or   {input? true shortcuts []
                             log [] limit 10
                             charge? false}
                      :as   options}]
  (let [extra-state {:charge?   charge?
                     ;;TODO: Support?
                     :input?    input?
                     :shortcuts shortcuts
                     :log       log
                     :limit     limit}]
    (assoc starting-state $ extra-state)))

(defn- charge-interpret [roll]
  (let [results (frequencies roll)
        max-val (apply max roll)]
    (cond
      (> (get results 6 0) 1) "Critical success!"
      (> max-val 5) "Success!"
      (> max-val 3) "Success, but with a consequence"
      :else "Consequence!")))

(defn- build-log [charge?
                  {:as   roll
                   :keys [::dice/text ::dice/roll ::dice/result]}]
  {:label   (if charge?
             (charge-interpret roll)
             text)
   :text   (str text " => " roll)
   :roll   roll
   :result result})

(defn roll! [{{:keys [log charge? limit]} $
              :as                         game}
             {:keys [formula] :as params}]
  ;; Note: Currently only single roll support
  (let [result  (->> (dice/parse-and-roll formula)
                           first
                           (build-log charge?))
        new-log (if limit
                        (take limit (cons result log))
                        (cons result log))]
    (-> game
        (assoc-in [$ :log] new-log)
        (assoc-in [$ :last] result))))

(defn- build-dice-button [{:keys [text formula]}]
  {:text   text
   :action :roll
   :params {:formula formula}})

(defn render-dice-bag-display [{{:keys [shortcuts last log]} $
                                :as                             state}]
  (let [buttons (mapv build-dice-button shortcuts)]
    (-> state
        (update :display assoc :dice-bag {:current last
                                          :log     log
                                          :actions buttons}))))
