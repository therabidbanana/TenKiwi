(ns tenkiwi.gamemasters.ftq
  "FTQ is a gamemaster supporting Descended by the Queen games"
  (:require [tenkiwi.util :as util :refer [inspect push-uniq]]
            [tenkiwi.rules.player-order :as player-order]
            [tenkiwi.rules.image-card :as image-card]
            [tenkiwi.rules.prompt-deck :as prompt-deck]
            [tenkiwi.rules.x-card :as x-card]
            ))

(def valid-active-actions #{:pass :discard :done :x-card :end-game :next-queen :previous-queen :leave-game})
(def valid-inactive-actions #{:x-card :undo :leave-game})

(defn valid-action? [active? action]
  (if active?
    (valid-active-actions action)
    (valid-inactive-actions action)))

(def done-action
  {:action :done
   :text   "Finish Turn"})

(def leave-game-action
  {:action  :leave-game
   :confirm true
   :text    "End Game Now"})

(def next-queen-action
  {:action :next-queen
   :class  :next-button
   :text   ">"})

(def previous-queen-action
  {:action :previous-queen
   :class  :previous-button
   :text   "<"})

(def discard-action
  {:action :discard
   :text   "[X] Discard this..."})

(def end-game-action
  {:action  :end-game
   :text    "End the Game"})

;; TODO: XSS danger?
(defn waiting-for
  [{:keys [user-name]}]
  {:id    "waiting"
   :state :inactive
   :text  (str "It is " user-name "'s turn...")})

(defn render-active-display [{:keys [display]
                              :as   state}]
  (let [{:keys [card
                active-player
                next-player
                x-card-active?]} display
        next-state               (or (:type card) :intro)
        pass                     {:action :pass
                                  :text   (str "Pass card to " (:user-name next-player))}
        next-actions             (case next-state
                                   :end    [pass end-game-action]
                                   :intro  [done-action pass]
                                   :prompt [done-action pass])]
    (assoc state
           :active-display
           (merge display
                  {:available-actions valid-active-actions
                   :extra-actions     (case next-state
                                        :end    [leave-game-action]
                                        :intro  [next-queen-action previous-queen-action leave-game-action]
                                        :prompt [leave-game-action])
                   :actions           (if x-card-active?
                                        (push-uniq next-actions discard-action)
                                        next-actions)}))))

(defn render-inactive-display [{:keys [display]
                               :as   state}]
  (let [waiting (waiting-for (:active-player display))]
    (assoc state
           :inactive-display
           (merge display
                  {:card              waiting
                   :available-actions valid-inactive-actions
                   :extra-actions     [leave-game-action]}))))

;; TODO: Remove when fully moved to new display system
(defn render-backwards-compatibility [{:as state
                                       {:keys [active-player
                                               queen]} :display}]
  (assoc state
         :queen queen
         :active-player active-player))

(defn render-game-display [next-game]
  (let [next-card       (prompt-deck/active-card next-game)
        next-state      (:type next-card)]
    (-> (assoc next-game :state next-state)
        player-order/render-display
        prompt-deck/render-display
        image-card/render-display
        x-card/render-display
        render-active-display
        render-inactive-display
        render-backwards-compatibility)))

(defn build-draw-deck [decks card-count]
  (into []
        (concat (:intro decks)
                (take card-count (shuffle (:prompt decks)))
                [(first (shuffle (:end decks)))])))


(defn start-game [room-id
                  {:keys [game-url]
                   :or   {}}
                  {:keys [players]
                   :as   game}]
  (let [decks         (util/gather-decks game-url)
        card-count    (+ 21 (rand 10))
        initial-state (-> {}
                          (prompt-deck/initial-state {:deck (build-draw-deck decks card-count)})
                          (player-order/initial-state {:players players})
                          (x-card/initial-state {})
                          (image-card/initial-state {:images    (:image decks)
                                                     :image-key :queen}))
        first-player  (player-order/active-player initial-state)
        next-player   (player-order/next-player initial-state)
        new-game      (merge {:game-type :ftq}
                             initial-state)]
    (render-game-display new-game)))

(defn finish-card [game]
  (-> game
      player-order/activate-next-player!
      x-card/reset-x-card!
      prompt-deck/draw-next-card!
      render-game-display))

(defn previous-queen [game]
  (-> (image-card/previous-image! game)
      render-game-display))

(defn next-queen [game]
  (-> (image-card/next-image! game)
      render-game-display))

(defn discard-card [game]
  (let [next-game       (-> game
                            x-card/reset-x-card!
                            prompt-deck/draw-next-card!)
        next-card       (prompt-deck/active-card next-game)]
    ;; Don't allow discard if deck empty
    (if next-card
      (render-game-display next-game)
      game)))

(defn pass-card [game]
  (-> game
      player-order/activate-next-player!
      render-game-display))

(defn x-card [game]
  (-> (x-card/activate-x-card! game)
      render-game-display))

(defn end-game [game]
  nil)

(defn tick-clock [game]
  ;; No-op
  game)

(defn take-action [{:keys [uid room-id action]} {:keys [game]}]
  (let [{:keys [active-player
                active-display
                state]
         :as   game}   game
        current-card   (:card active-display)
        active-player? (= (:id active-player) uid)
        valid?         (valid-action? active-player? action)
        do-next-state  (case action
                         :next-queen     next-queen
                         :previous-queen previous-queen
                         :done           finish-card
                         :x-card         x-card
                         :discard        discard-card
                         :pass           pass-card
                         :tick-clock     tick-clock
                         ;; TODO allow players to leave game without ending
                         ;;; change action text
                         :leave-game     end-game
                         :end-game       end-game)]
    ;; (println next-state)
    (do-next-state game)))

(comment
  (def fake-state {:rooms {1 {:playes [{:id "a"}]}}})

  (start-game (atom fake-state) 1)

  )
