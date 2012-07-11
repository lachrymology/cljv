(ns cljv.parser
  (:require [cljv.java :as java]
            [clojure.string :as string]
            [cljs.compiler :as common]))

(defmulti parse :op)

(def ^:dynamic *position* nil)

(defmacro parse-wrap [env & body]
  `(let [env# ~env]
     (with-meta
       ~@body
       {:context (:context env#)})))

(defn emit-block
  [context statements ret]
  (when statements
    (emits statements))
    (emit ret))

(defmethod parse :constant
  [{:keys [form env]}]
  (when-not (= :statement (:context env))
    (parse-wrap
     env
     {:node  :constant
      :value form})))

(defmethod parse :no-op [_])
(defmethod parse :var [_])
(defmethod parse :meta [_])
(defmethod parse :map [_])
(defmethod parse :vector [_])
(defmethod parse :set [_])
(defmethod parse :if [_])
(defmethod parse :throw [_])
(defmethod parse :def [_])
(defmethod parse :fn [_])
(defmethod parse :do [_])
(defmethod parse :try* [_])
(defmethod parse :let [_])
(defmethod parse :recur [_])
(defmethod parse :letfn [_])
(defmethod parse :invoke [_])

(defmethod parse :new
  [{:keys [ctor args env]}]
  (parse-wrap env
    {:node :new
     :args args}))

(defmethod parse :set! [_])
(defmethod parse :ns [_])
(defmethod parse :deftype* [_])
(defmethod parse :defrecord* [_])
(defmethod parse :dot [_])
(defmethod parse :java [_])
(defmethod parse :default [_])

