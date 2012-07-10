(ns cljv.test.emitter
  (:refer-clojure :exclude (munge))
  (:use [cljv.emitter]
        [clojure.test]
        [cljv.test.common]))

(deftest cljv-test-emitter
  (defer "testing of the cljv Java emitter."))
