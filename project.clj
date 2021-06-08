(defproject tenkiwi "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.439" :scope "provided"]
                 [com.cognitect/transit-clj "1.0.324"]
                 [com.cognitect/transit-cljs "0.8.269"]
                 [ring "1.7.1"]
                 [ring/ring-defaults "0.3.2"]
                 [bk/ring-gzip "0.3.0"]
                 [radicalzephyr/ring.middleware.logger "0.6.0"]
                 [clj-logging-config "1.9.12"]
                 [environ "1.1.0"]
                 [com.stuartsierra/component "0.3.2"]
                 [org.danielsz/system "0.4.6"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [http-kit "2.5.0"]
                 [compojure "1.6.1"]
                 [cljsjs/react "16.13.1-0"]
                 [cljsjs/react-dom "16.13.1-0"]
                 [re-frame "1.1.2"]
                 [lambdaisland/garden-watcher "0.3.3"]


                 [markdown-to-hiccup "0.6.2"]
                 [com.taoensso/sente "1.16.0"]
                 [devcards "0.2.5"]
                 ]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-environ "1.1.0"]]

  :min-lein-version "2.6.1"

  :source-paths ["src/clj" "src/cljs" "src/cljc"]

  :test-paths ["test/clj" "test/cljc"]

  :clean-targets ^{:protect false} [:target-path :compile-path "resources/public/js" "dev-target"]

  :uberjar-name "tenkiwi.jar"

  ;; Use `lein run` if you just want to start a HTTP server, without figwheel
  :main tenkiwi.application

  ;; nREPL by default starts in the :main namespace, we want to start in `user`
  ;; because that's where our development helper functions like (go) and
  ;; (browser-repl) live.
  :repl-options {:init-ns user
                 :timeout 300000}

  :cljsbuild {:builds
              [{:id "app"
                :source-paths ["src/cljs" "src/cljc" "dev"]

                :figwheel {:on-jsload "tenkiwi.system/reset"}

                :compiler {:main cljs.user
                           :asset-path "js/compiled/out"
                           :output-to "dev-target/public/js/compiled/tenkiwi.js"
                           :output-dir "dev-target/public/js/compiled/out"
                           :source-map-timestamp true}}
               {:id           "devcards"
                :source-paths ["src"]
                :figwheel     { :devcards true } ;; <- note this
                :compiler     {:main                 "tenkiwi.devcards"
                               :asset-path           "js/devcards_out/out"
                               :output-to            "dev-target/public/js/devcards_out/tenkiwi_devcards.js"
                               :output-dir           "dev-target/public/js/devcards_out/out"
                               :source-map-timestamp true}}
               {:id "test"
                :source-paths ["src/cljs" "test/cljs" "src/cljc" "test/cljc"]
                :compiler {:output-to "dev-target/public/js/compiled/testable.js"
                           :main tenkiwi.test-runner
                           :optimizations :none}}

               {:id "min"
                :source-paths ["src/cljs" "src/cljc"]
                :jar true
                :compiler {:main tenkiwi.system
                           :output-to "resources/public/js/compiled/tenkiwi.js"
                           :output-dir "target"
                           :source-map-timestamp true
                           :optimizations :advanced
                           :closure-defines {goog.DEBUG false}
                           :pretty-print false}}]}

  ;; When running figwheel from nREPL, figwheel will read this configuration
  ;; stanza, but it will read it without passing through leiningen's profile
  ;; merging. So don't put a :figwheel section under the :dev profile, it will
  ;; not be picked up, instead configure figwheel here on the top level.

  :figwheel {;; :http-server-root "public"       ;; serve static assets from resources/public/
             ;; :server-port 3449                ;; default
             ;; :server-ip "127.0.0.1"           ;; default
             :css-dirs ["resources/public/css"]  ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process. We
             ;; don't do this, instead we do the opposite, running figwheel from
             ;; an nREPL process, see
             ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
             ;; :nrepl-port 7888

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             :server-logfile "log/figwheel.log"}

  :doo {:build "test"}

  :profiles {:dev
             {:dependencies [[figwheel "0.5.18"]
                             [figwheel-sidecar "0.5.18"]
                             [cider/piggieback "0.5.2"]
                             [nrepl/nrepl "0.8.2"]
                             [lein-doo "0.1.11"]
                             [reloaded.repl "0.2.4"]]

              :plugins [[lein-figwheel "0.5.18"]
                        [lein-doo "0.1.11"]
                        [cider/cider-nrepl "0.25.5"]]

              :source-paths ["dev"]
              :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl
                                                cider.nrepl/wrap-format
                                                cider.nrepl/wrap-apropos
                                                ]}}

             :uberjar
             {:source-paths ^:replace ["src/clj" "src/cljc"]
              :prep-tasks ["compile"
                           ["cljsbuild" "once" "min"]
                           ["run" "-m" "garden-watcher.main" "tenkiwi.styles"]]
              :hooks []
              :omit-source true
              :aot :all}})
