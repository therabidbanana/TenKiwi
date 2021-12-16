(ns tenkiwi.config
  (:require [environ.core :refer [env]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.logger :refer [wrap-with-logger]]))

(defn config []
  {:http-port  (Integer. (or (env :port) 10555))
   :games-library (or (env :games-library)
                      "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ1Uj9bFkSyrifXYBFEHeXB39fzwJNzM73HipLYDlWGEHSaUTVAob7nXUIEAxumYb_SjeC9bivjpEMp/pub?gid=0&single=true&output=tsv")
   :database-uri (or (env :database-url)
                     "postgres://tenkiwi:secretPasskiwi@localhost:5432/tenkiwi")
   :middleware [[wrap-defaults site-defaults]
                wrap-with-logger
                wrap-gzip]})
