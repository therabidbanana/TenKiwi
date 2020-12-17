(ns tenkiwi.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [tenkiwi.core-test]
   [tenkiwi.common-test]))

(enable-console-print!)

(doo-tests 'tenkiwi.core-test
           'tenkiwi.common-test)
