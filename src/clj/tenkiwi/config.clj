(ns tenkiwi.config
  (:require [environ.core :refer [env]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.logger :refer [wrap-with-logger]]))

(defn config []
  {:http-port  (Integer. (or (env :port) 10555))
   :games-library (or (env :games-library)
                      ;; Dummy library with the default games
                      "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ1Uj9bFkSyrifXYBFEHeXB39fzwJNzM73HipLYDlWGEHSaUTVAob7nXUIEAxumYb_SjeC9bivjpEMp/pub?gid=0&single=true&output=tsv")
   :database-uri (or (env :database-url)
                     "postgres://tenkiwi:secretPasskiwi@localhost:5432/tenkiwi")
   :s3-creds {:access-key (or (env :s3-access-key))
              :secret-key (or (env :s3-secret-key))
              :bucketname (or (env :s3-bucket-name) "tenkiwi-test")
              :endpoint (or (env :s3-endpoint)
                            "https://us-southeast-1.linodeobjects.com")}
   :middleware [[wrap-defaults site-defaults]
                wrap-with-logger
                wrap-gzip]})
