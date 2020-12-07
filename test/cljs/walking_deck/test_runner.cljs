(ns walking-deck.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [walking-deck.core-test]
   [walking-deck.common-test]))

(enable-console-print!)

(doo-tests 'walking-deck.core-test
           'walking-deck.common-test)
