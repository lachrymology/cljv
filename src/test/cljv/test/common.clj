(ns cljv.test.common
  (:use clojure.test))

(defn defer
  [msg]
  (println (str "DEFERRING: " msg))
  (is (= 1 1)))
