(ns tenkiwi.gamemasters.host
  "The host is in charge of moving users back and forth to rooms"
  (:require [tenkiwi.gamemasters.ftq :as ftq]
            [tenkiwi.gamemasters.debrief :as debrief]
            [tenkiwi.gamemasters.walking-deck-v2 :as walking-deck-v2]
            [tenkiwi.gamemasters.oracle :as oracle]
            [tenkiwi.gamemasters.opera :as opera]
            [tenkiwi.gamemasters.wretched :as wretched]
            [tenkiwi.instar :refer [transform]]
            [tenkiwi.room-atoms :as room-atoms]
            [tenkiwi.util :as util :refer [inspect]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
))

(def home-room :home)
(defn home-room? [room] (= home-room (or room home-room)))
(defn valid-game? [type] (#{:ftq :walking-deck :debrief :oracle} type))

;; Drop hidden game state (mainly for performance)
(defn- possibly-remove-keys [m]
  (if (map? m)
    (let [result (transform m [:game #"^-"] dissoc)]
      result)
    m))

(defn ->players [{:keys [chsk-send!]} uids message]
  (doseq [uid uids]
    (let [mapped-msg (mapv possibly-remove-keys message)]
      (chsk-send! uid mapped-msg))))

(defn ->player [system uid message]
  (->players system [uid] message))

(defn ->room
  [{:keys [register] :as system} room-id message]
  (let [room (-> register :open-rooms deref
                 (get-in [room-id]))]
    (if room
      (if-let [players (-> room deref (get-in [:players]))]
        (->players system (map :id players) message))
      (if-let [players (-> register :world deref
                           (get-in [:lobbies room-id :players]))]
        (->players system (map :id players) message)))))

(defn get-player-location [register uid]
  (let [world-atom (:world register)]
    (get-in @world-atom [:players uid])))

(defn get-room [register room-id]
  (let [world-atom (:world register)
        open-rooms (:open-rooms register)
        lobby      (get-in @world-atom [:lobbies room-id])
        room       (if-let [room (get-in @open-rooms [room-id])]
                     (deref room))]
    (or lobby room)))

(defn delete-room-if-empty [room]
  (if (empty? (:players room))
    nil
    room))

(defn send-player-home [world-state uid]
  (let [player-location (get-in world-state [:players uid])
        filter-user (fn [list] (remove #(= uid (:id %)) list))]
    (if (home-room? player-location)
      world-state
      (-> world-state
          (update-in [:players] assoc uid home-room)
          (update-in [:lobbies player-location :players] filter-user)
          (update-in [:lobbies player-location] delete-room-if-empty)))))

(def GAME-LIBRARY
  (or (env :games-library)
      "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ1Uj9bFkSyrifXYBFEHeXB39fzwJNzM73HipLYDlWGEHSaUTVAob7nXUIEAxumYb_SjeC9bivjpEMp/pub?gid=0&single=true&output=tsv"))

(defn open-and-join-lobby
  ([register uid lobby-id user-info]
   (let [world-atom (:world register)
         host-codes (into #{} (:unlock-codes user-info))
         available-games (filter (fn [{:keys [code]}]
                                   (or (empty? code) (= lobby-id code)
                                       (host-codes code)))
                                 (util/read-spreadsheet-data GAME-LIBRARY util/normalize-card))
         valid-modes (into #{:ftq :debrief :wretched :opera}
                           (map :type available-games))
         room (or (get-room register lobby-id)
                  {:id (str uid "/" lobby-id)
                   :room-code lobby-id
                   :atom-id  (str uid "/" lobby-id)
                   :valid-modes valid-modes
                   :host-id uid
                   :available-games available-games
                   :players []
                   :opened-at (tc/to-long (t/now))})
         ;;; TODO - bounce if user joining started game (limit trolling)
         ;;; TODO - concurrency bug room join
         new-room (update-in room [:players] conj user-info)]
     (room-atoms/open-room register (:atom-id new-room))
     (doto world-atom
      (swap! update-in [:players] assoc uid lobby-id)
      (swap! update-in [:player-info] assoc uid user-info)
      (swap! update-in [:lobbies] assoc lobby-id new-room)))))

(defn new-arrival!
  "Called whenever a new uid arrives"
  [{:as system :keys [register chsk-send!]} uid]
  (let [player-location   (get-player-location register uid)
        invalid-redirect? (or (= uid :taoensso.sente/nil-uid) (home-room? player-location))]
    (println chsk-send! uid player-location)
    (if-not invalid-redirect?
      (if-let [room (get-room register player-location)]
        (->player system uid [:->user/room-joined! room])
        (if (clojure.string/includes? player-location uid)
          (do
            (room-atoms/open-room register player-location)
            (if-let [room (get-room register player-location)]
              (->player system uid [:->user/room-joined! room]))))))))

(defn game-starter [game-name room-id params]
  (cond
    (home-room? room-id) nil
    :else
    (case game-name
      :debrief (partial debrief/start-game room-id params)
      :oracle (partial oracle/start-game room-id params)
      :walking-deck-v2 (partial walking-deck-v2/start-game room-id params)
      :ftq (partial ftq/start-game room-id params)
      :opera (partial opera/start-game room-id params)
      :wretched (partial wretched/start-game room-id params)
      nil)))

(defn select-custom-game [room-id {:keys []
                                   :as   params
                                   :or   {}}
                          {:keys [valid-modes] :as room}]
  (let [new-game {:game-type     :custom
                  :configuration {:params (assoc params :game-type :ftq :game-url "" :custom-play? true)
                                  :inputs [{:type    :select
                                            :label   "Game Mode"
                                            :name    :game-type
                                            :options (mapv #(hash-map :value (:id %) :name (:title %))
                                                           (filterv
                                                            #(valid-modes (:id %))
                                                            [{:id    :ftq
                                                              :title "Descended from the Queen"}
                                                             {:id    :debrief
                                                              :title "Debrief"}
                                                             {:id    :opera
                                                              :title "Opera"}
                                                             {:id    :wretched
                                                              :title "Wretched and Alone"}
                                                             {:id    :walking-deck-v2
                                                              :title "Walking Deck"}
                                                             ]))}
                                           {:type  :text
                                            :label "Tab Separated Values URL"
                                            :name  :game-url}
                                           ]}
                  :game-url      ""}]
    new-game))

(defn game-selector [game-name room-id params]
  (cond
    (home-room? room-id) nil
    :else
    (case game-name
      :opera (partial opera/select-game room-id params)
      :custom (partial select-custom-game room-id params)
      :wretched (partial wretched/select-game room-id params)
      :walking-deck-v2 (partial walking-deck-v2/select-game room-id params)
      nil    (constantly nil)
      (constantly {:configuration {:params params}}))))

(defn game-action [game-name {:keys [uid room-id] :as action}]
  (cond
    (home-room? room-id) nil
    :else
    (case game-name
      :debrief (partial debrief/take-action action)
      :oracle (partial oracle/take-action action)
      :walking-deck-v2 (partial walking-deck-v2/take-action action)
      :ftq (partial ftq/take-action action)
      :opera (partial opera/take-action action)
      :wretched (partial wretched/take-action action)
      nil)))

(defn log-unless-timekeeper [output uid]
  (if-not (#{:timekeeper} uid)
    (println (select-keys output [:room-id :player-order :act]))))

(defn shut-down-room [{:keys [register] :as system} game room-id]
  (let [player-ids (map :id (:players game))
        send-room-home     (fn [world-state]
                             (reduce #(update-in %1 [:players %2] home-room)
                                     world-state
                                     player-ids))]
    (swap! (:world register) send-room-home)
    (room-atoms/close-room register room-id true)
    (->players system player-ids [:->user/booted!])))

(defn update-room-state! [current-game system room-id mutator]
  (let [response     (mutator current-game)
        new-game     (dissoc response :broadcasts)]
    (if new-game
      (do
        (doseq [broadcast (:broadcasts response)]
          (->room system room-id broadcast))
        (assoc-in current-game [:game] new-game))
      (shut-down-room system current-game room-id))))

(defn update-room-setup! [world-state system room-id mutator]
  (let [current-game (get-in world-state [:lobbies room-id])
        response (mutator current-game)
        new-game (dissoc response :broadcasts)]
    (doseq [broadcast (:broadcasts response)]
      (->room system room-id broadcast))
    (assoc-in world-state [:lobbies room-id :game-setup] new-game)))

(defn leave-room!
  [{:as system :keys [register]} uid]
  (swap! (:world register) send-player-home uid))

(defn join-room!
  "Called when a player tries to join an existing room"
  [{:as system :keys [register]} uid room-code {:as join-info :keys [user-name]}]
  (let [world           (:world register)
        player-location (get-player-location register uid)]
    (if-not (home-room? player-location)
      (leave-room! system uid))
    (open-and-join-lobby register uid room-code join-info)
    (let [player-location (get-player-location register uid)
          room            (get-room register player-location)]
      (->player system uid [:->user/room-joined! room])
      (println "send to " player-location)
      (->room system player-location [:->room/user-joined! room]))))

(defn select-game!
  "Called to trigger a game start by host"
  [{:as system :keys [register]} uid {:keys [game-type
                                             params]}]
  (let [world   (:world register)
        lobby-id (get-player-location register uid)
        mutator (game-selector game-type lobby-id params)]
    (if mutator
      (let [output    (swap! world update-room-setup! system lobby-id mutator)
            new-state (get-in output [:lobbies lobby-id :game-setup])]
        (log-unless-timekeeper new-state uid)
        (->room system lobby-id [:->game/selected! (get-room register lobby-id)])))))

(defn- move-lobby-to-room [world-state lobby-id room-id]
  (let [all-players (map :id (get-in world-state [:lobbies lobby-id :players]))
        updated     (reduce #(assoc-in %1 [:players %2] room-id)
                            world-state
                            all-players)]
    (update updated :lobbies dissoc lobby-id)))

(defn start-game!
  "Called to trigger a game start by host"
  [{:as system :keys [register]} uid {:keys [game-type
                                             params]}]
  (let [lobby-id  (get-player-location register uid)
        lobby     (-> register :world deref (get-in [:lobbies lobby-id]))
        atom-id   (:atom-id lobby)
        register  (room-atoms/open-room register atom-id)
        room-atom (-> register :open-rooms deref (get atom-id))
        mutator   (game-starter game-type lobby-id params)]
    (if mutator
      (let [new-state (reset! room-atom (assoc lobby :game (mutator lobby)))
            world     (swap! (:world register) move-lobby-to-room lobby-id atom-id)]
        (log-unless-timekeeper new-state uid)
        (->players system (map :id (:players lobby))
                   [:->game/started! new-state])))))

(defn- run-action
  "Do the work of triggering an action / broadcasting results, used by clock and
  action calls"
  [{{:keys [world] :as register} :register
    :as system}
                  {:as action :keys [uid room-id]}]
  (let [room         (get-room register room-id)
        current-game (get-in room [:game :game-type])
        mutator      (game-action current-game action)
        room-atom    (-> register :open-rooms deref (get room-id))]
    (if mutator
      (let [output    (swap! room-atom update-room-state! system room-id mutator)
            new-state (get-in output [:game])]
        (log-unless-timekeeper new-state uid)
        (->room system room-id [:->game/changed! (get-room register room-id)])))))

(defn tick-clock!
  "Called by the system to tick all game clocks"
  [{:as system :keys [register]}]
  (let []
    (doseq [room (-> register :open-rooms deref keys)]
      ;; TODO - handle no-ops, cheaper ticks
      ;; - tick clock is expensive if triggering full rerender on mobile
     #_(run-action system {:room-id room
                         :action  :tick-clock
                         :uid     :timekeeper}))))

(defn take-action!
  "Called to trigger a game start by host"
  [{:as system :keys [register]} uid action]
  (let [world           (:world register)
        player-location (get-player-location register uid)]
    (run-action system  (assoc action :room-id player-location :uid uid))))

(defn boot-player!
  [{:as system :keys [register]} uid]
  (let [world           (:world register)
        player-location (get-player-location register uid)]
    (swap! world send-player-home uid)
    (let [room (get-room register player-location)]
      (->player system uid [:->user/booted!])
      (->room system player-location [:->room/user-left! room]))))
