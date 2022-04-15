(ns tenkiwi.rules.undoable
  (:require
   [tenkiwi.util :as util :refer [inspect]]
   [editscript.core :as e]
   )
  )


(def $ :-undos)

(defn initial-state [starting-state
                     {:keys [skip-keys]
                      :or   {skip-keys [:display]}
                      :as   options}]
  (let [extra-state  {:checkpoints []
                      :skip-keys (conj (into [] skip-keys) $)}]
    (assoc starting-state $ extra-state)))


(defn checkpoint! [{:as state
                    {:keys [skip-keys]} $}
                   old-state]
  (let [old (apply dissoc old-state skip-keys)
        new (apply dissoc state skip-keys)
        ;; How do you turn new back to old?
        diff (e/diff new old)]
    (update-in state [$ :checkpoints] conj diff)))

(defn undo! [{:as state
              {:keys [checkpoints]} $}]
  (-> state
      (e/patch (last checkpoints))
      (update-in [$ :checkpoints] pop)))
