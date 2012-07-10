(ns cljv.parser.constants)

(defmulti parse-constant class)

(defmethod parse-constant :default [_] nil)

