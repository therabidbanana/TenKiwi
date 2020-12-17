(ns tenkiwi.db)

(def default-db
  {:name "re-frame"
   :user {:user-name ""
          :current-room nil}
   :join {:user-name ""
          :room-code ""}})
