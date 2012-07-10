(ns cljv.emitter.constants)

(defmulti emit-constant class)

(defmethod emit-constant :default [_] nil)

