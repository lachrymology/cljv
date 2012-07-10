(ns cljv.emitter
  (:refer-clojure :exclude (munge))
  (:require [cljv.java :as java]
            [cljs.compiler :as common]))

(def munge #(common/munge % java/reserved))
(def comma-sep #'common/comma-sep)
(def escape-char #'common/escape-char)
(def escape-string #'common/escape-string)
(def wrap-in-double-quotes #'common/wrap-in-double-quotes)

(defn emit [_])

