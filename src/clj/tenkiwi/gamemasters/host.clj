(ns tenkiwi.gamemasters.host
  "The host is in charge of moving users back and forth to rooms"
  (:require [tenkiwi.gamemasters.ftq :as ftq]
            [tenkiwi.gamemasters.debrief :as debrief]
            [tenkiwi.gamemasters.walking-deck-v2 :as walking-deck-v2]
            [tenkiwi.gamemasters.oracle :as oracle]
            [tenkiwi.gamemasters.opera :as opera]
            [tenkiwi.gamemasters.wretched :as wretched]
            [tenkiwi.instar :refer [transform]]
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
  [{:keys [register] :as system} room message]
  (if-let [players (-> register :world deref
                          (get-in [:rooms room :players]))]
    (->players system (map :id players) message)))

(defn get-player-location [world-atom uid]
  (get-in @world-atom [:players uid]))

(defn get-room [world-atom room-id]
  (get-in @world-atom [:rooms room-id]))

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
          (update-in [:rooms player-location :players] filter-user)
          (update-in [:rooms player-location] delete-room-if-empty)))))

(def GAME-LIBRARY
  (or (env :games-library)
      "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ1Uj9bFkSyrifXYBFEHeXB39fzwJNzM73HipLYDlWGEHSaUTVAob7nXUIEAxumYb_SjeC9bivjpEMp/pub?gid=0&single=true&output=tsv"))

(defn set-player-room
  ([world-atom uid room-id]
   (let [user-info (get-in @world-atom [:player-info uid])]
     (set-player-room world-atom uid room-id user-info)))
  ([world-atom uid room-id user-info]
   (let [host-codes (into #{} (:unlock-codes user-info))
         available-games (filter (fn [{:keys [code]}]
                                   (or (empty? code) (= room-id code)
                                       (host-codes code)))
                                 (util/read-spreadsheet-data GAME-LIBRARY util/normalize-card))
         room (or (get-room world-atom room-id)
                  {:id room-id
                   :room-code room-id
                   :host-id uid
                   :available-games available-games
                   :players []
                   :opened-at (tc/to-long (t/now))})
         _ (println room)
         ;;; TODO - bounce if user joining started game (limit trolling)
         ;;; TODO - concurrency bug room join
         new-room (update-in room [:players] conj user-info)]
     (doto world-atom
      (swap! update-in [:players] assoc uid room-id)
      (swap! update-in [:player-info] assoc uid user-info)
      (swap! update-in [:rooms] assoc room-id new-room)))))

(defn new-arrival!
  "Called whenever a new uid arrives"
  [{:as system :keys [register chsk-send!]} uid]
  (let [player-location   (get-player-location (:world register) uid)
        invalid-redirect? (or (= uid :taoensso.sente/nil-uid) (home-room? player-location))]
    (println chsk-send! uid player-location)
    (if-not invalid-redirect?
      (if-let [room (get-room (:world register) player-location)]
        (->player system uid [:->user/room-joined! room])))))

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

(defn game-selector [game-name room-id params]
  (cond
    (home-room? room-id) nil
    :else
    (case game-name
      :opera (partial opera/select-game room-id params)
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

(defn update-room-state! [world-state system room-id mutator]
  (let [current-game (get-in world-state [:rooms room-id])
        response (mutator current-game)
        new-game (dissoc response :broadcasts)]
    (doseq [broadcast (:broadcasts response)]
      (->room system room-id broadcast))
    (assoc-in world-state [:rooms room-id :game] new-game)))

(defn update-room-setup! [world-state system room-id mutator]
  (let [current-game (get-in world-state [:rooms room-id])
        response (mutator current-game)
        new-game (dissoc response :broadcasts)]
    (doseq [broadcast (:broadcasts response)]
      (->room system room-id broadcast))
    (assoc-in world-state [:rooms room-id :game-setup] new-game)))

(defn leave-room!
  [{:as system :keys [register]} uid]
  (swap! (:world register) send-player-home uid))

(defn join-room!
  "Called when a player tries to join an existing room"
  [{:as system :keys [register]} uid room-code {:as join-info :keys [user-name]}]
  (let [world           (:world register)
        player-location (get-player-location world uid)]
    (if-not (home-room? player-location)
      (leave-room! system uid))
    (set-player-room world uid room-code join-info)
    (let [player-location (get-player-location world uid)
          room        (get-room world player-location)]
      (->player system uid [:->user/room-joined! room])
      (println "send to " player-location)
      (->room system player-location [:->room/user-joined! room]))))

(defn select-game!
  "Called to trigger a game start by host"
  [{:as system :keys [register]} uid {:keys [game-type
                                             params]}]
  (let [world   (:world register)
        room-id (get-player-location world uid)
        mutator (game-selector game-type room-id params)]
    (if mutator
      (let [output    (swap! world update-room-setup! system room-id mutator)
            new-state (get-in output [:rooms room-id :game-setup])]
        (log-unless-timekeeper new-state uid)
        (->room system room-id [:->game/selected! (get-room world room-id)])))))

(defn start-game!
  "Called to trigger a game start by host"
  [{:as system :keys [register]} uid {:keys [game-type
                                             params]}]
  (let [world   (:world register)
        room-id (get-player-location world uid)
        mutator (game-starter game-type room-id params)]
    (if mutator
      (let [output    (swap! world update-room-state! system room-id mutator)
            new-state (get-in output [:rooms room-id :game])]
        (log-unless-timekeeper new-state uid)
        (->room system room-id [:->game/started! (get-room world room-id)])))))

(defn- run-action
  "Do the work of triggering an action / broadcasting results, used by clock and
  action calls"
  [{{:keys [world]} :register :as system}
                  {:as action :keys [uid room-id]}]
  (let [room         (get-room world room-id)
        current-game (get-in room [:game :game-type])
        mutator      (game-action current-game action)]
    (if mutator
      (let [output    (swap! world update-room-state! system room-id mutator)
            new-state (get-in output [:rooms room-id :game])]
        (log-unless-timekeeper new-state uid)
        (->room system room-id [:->game/changed! (get-room world room-id)])))))

(defn tick-clock!
  "Called by the system to tick all game clocks"
  [{:as system :keys [register]}]
  (let []
    (doseq [room (-> register :world deref :rooms keys)]
      ;; TODO - handle no-ops, cheaper ticks
      ;; - tick clock is expensive if triggering full rerender on mobile
     (run-action system {:room-id room
                         :action  :tick-clock
                         :uid     :timekeeper}))))

(defn take-action!
  "Called to trigger a game start by host"
  [{:as system :keys [register]} uid action]
  (let [world           (:world register)
        player-location (get-player-location world uid)]
    (run-action system (assoc action :room-id player-location :uid uid))))

(defn boot-player!
  [{:as system :keys [register]} uid]
  (let [world           (:world register)
        player-location (get-player-location world uid)]
    (swap! world send-player-home uid)
    (let [room (get-room world player-location)]
      (->player system uid [:->user/booted!])
      (->room system player-location [:->room/user-left! room]))))
