(ns walking-deck.styles
  (:require [garden-watcher.def :refer [defstyles]]))

(def base
  (list
   [:h1 {:text-decoration "underline"}]
   [:input {:width "100%"
            :font-size "2rem"
            :margin "0.5rem"}]
   [:form {:max-width "80%"
           :margin "0 auto"
           :padding "1rem"}]
   [:button {:font-size "2rem"}]
   [:body {:color "#eeeef0"
           :font-size "1.2rem"
           :font-family "Tahoma"
           :background-color "#223"}]))

(def game-table
  (list
   ;; TODO: This styling is a tangled mess
   [:.game-table {:margin-bottom "12rem"}
    [:.extras
     {:position 'relative
      :z-index  '0}
     [:img {:display    "block"
            :max-height "60vh"
            :margin     "0 auto"}]
     [:a.button {:width      "80%"
                 :font-size  "1.1rem"
                 :background "#ccc"
                 :color      "#111"
                 :cursor     'pointer
                 :margin     "1rem"
                 :display    'block
                 :padding    "1rem"}]
     [:.next-button {:position   'absolute
                     :text-align 'center
                     :height     "70%"
                     :top        "0"
                     :right      "3rem"
                     }
      [:a.button {:margin           "3rem 0"
                  :padding          "3rem 0.8rem"
                  :background-color "#000"
                  :color            'white
                  :font-size        "5rem"
                  :opacity          "20%"}]]
     [:.previous-button {:position   'absolute
                         :text-align 'center
                         :height     "70%"
                         :top        "0"
                         :left       "3rem"}
      [:a.button {:margin           "3rem 0"
                  :padding          "3rem 0.8rem 3.8rem"
                  :background-color "#000"
                  :color            'white
                  :font-size        "5rem"
                  :opacity          "20%"}]]]
    [:.active-area {:text-align    "center"
                    :margin-bottom "9rem"}
     [:.x-card {:float            'right
                :font-size        "1.6rem"
                :background-color "#47a"
                :color            "white"}
      [:&.active {:background-color "red"
                  :color            "white"}]
      [:a {:display 'block
           :padding "0.9rem 0.7rem"
           :cursor  'pointer}]]]
    [:.actions {:position 'fixed
                :z-index  "1"
                :width    "100%"
                :bottom   "0px"}]
    [:.action {:width         "80%"
               :font-size     "1.4rem"
               :background    "#222"
               :border        "1px solid white"
               :cursor        'pointer
               :margin        "1rem auto"
               :border-radius "0.2rem"
               :box-shadow    "2px 2px 4px #123"}
     [:a {:display "block"
          :padding "1rem"}]]
    [:.card {:background-color "#efefef"
             :padding          "2.2rem 1.2rem"
             :margin           "0.5rem"
             :font-family      "Georgia"
             :font-size        "1.6rem"
             :color            "#224"
             :border-radius    "0.2rem"
             :border           "0.4rem solid #efefef"
             :box-shadow       "2px 2px 4px #123"}
     [:&.intro {:border-color "#aab"}]
     [:&.question {:border-color "#47a"}]
     [:&.x-carded {:border-color "red"}]]]))

(def lobby
  (list
   [:.lobby
    {:max-width "80%"
     :margin "0 auto"}

    [:li {:border "1px solid #bbb"
          :font-size "2rem"
          :list-style 'none
          :padding "0.6rem"}]
    [:.boot {:cursor 'pointer
             :padding "0.6rem 1rem"
             :margin-top "-0.6rem"
             :margin-right "-0.6rem"
             :overflow "hidden"
             :color "white"
             :background "red"
             :float 'right}]]))

(defstyles style
  (conj [] base lobby game-table))
