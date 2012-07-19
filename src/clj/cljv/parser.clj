(ns cljv.parser
  (:require [cljv.java :as java]
            [clojure.string :as string]
            [cljs.compiler :as common]))

(defmulti parse :op)

(def ^:dynamic *position* nil)

(defmethod parse :constant
  [{:keys [form env]}]
  {:node :constant
   :value form
   :context (:context env)})

(defmethod parse :no-op [_])

(defmethod parse :var
  [{:keys [info env]}]
  (let [n (:name info)]
    {:node :var
     :name n
     :namespace (namespace n)
     :context (:context env)
     :env env}))

(defmethod parse :meta
  [{:keys [expr meta env]}]
  {:node :meta
   :context (:context env)
   :expr expr
   :meta meta
   :env env})

(defmethod parse :map
  [{:keys [env simple-keys? keys vals]}]
  {:node :map
   :context (:context env)
   :env env
   :simple-keys? simple-keys?
   :keys keys
   :keys-sz (count keys)
   :vals vals
   :vals-sz (count vals)})

(defmethod parse :vector
  [{:keys [items env]}]
  {:node :vector
   :context (:context env)
   :env env
   :items items
   :items-sz (count items)})

(defmethod parse :set
  [{:keys [items env]}]
  {:node :set
   :context (:context env)
   :items items
   :items-sz (count items)
   :env env})

(defn get-tag [e]
  (or (-> e :tag)
      (-> e :info :tag)))

(defn infer-tag [e]
  (if-let [tag (get-tag e)]
    tag
    (case (:op e)
      :let (infer-tag (:ret e))
      :if (let [then-tag (infer-tag (:then e))
                else-tag (infer-tag (:else e))]
            (when (= then-tag else-tag)
              then-tag))
      :constant (case (:form e)
                  true 'boolean
                  false 'boolean
                  nil)
      nil)))

(defn safe-test? [e]
  (let [tag (infer-tag e)]
    (or (#{'boolean 'seq} tag)
        (when (= (:op e) :constant)
          (let [form (:form e)]
            (not (or (and (string? form) (= form ""))
                                          (and (number? form) (zero? form)))))))))

(defmethod parse :if
  [{:keys [test then else env unchecked]}]
  {:node :if
   :context (:context env)
   :test test
   :checked (not (or unchecked (safe-test? test)))
   :then then
   :else else
   :env env})

(defmethod parse :throw
  [{:keys [throw env]}]
  {:node :throw
   :context (:context env)
   :env env
   :throw throw})

(defmethod parse :def
  [{:keys [name init env doc export]}]
  {:node :def
   :context (:context env)
   :env env
   :name name
   :doc doc
   :jvdoc (:jsdoc init)
   :export export})

(defmethod parse :fn [_])
(defmethod parse :do [_])
(defmethod parse :try* [_])
(defmethod parse :let [_])
(defmethod parse :recur [_])
(defmethod parse :letfn [_])
(defmethod parse :invoke [_])

(defmethod parse :new
  [{:keys [ctor args env]}]
  {:node :new
   :args args
   :context (:context env)
   :env env})

(defmethod parse :set! [_])
(defmethod parse :ns [_])
(defmethod parse :deftype* [_])
(defmethod parse :defrecord* [_])
(defmethod parse :dot [_])
(defmethod parse :java [_])
(defmethod parse :default [_])

