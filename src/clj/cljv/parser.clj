(ns cljv.parser
  (:require [cljv.java :as java]
            [clojure.string :as string]
            [cljs.compiler :as common]))

;; TODO
;; - parse subnodes
;; - munge here?
;; - convenience macro with post-conditions


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

(defmethod parse :do
  [{:keys [statements ret env]}]
  {:node :do
   :context (:context env)
   :env env
   :statements statements
   :ret ret})

(defmethod parse :try*
  [{:keys [env try catch name finally]}]
  (assert (not= :constant (-> finally :ret :op))
          "finally block cannot contain constant")
  
  (let [context (:context env)]
    {:node :try*
     :context context
     :subcontext (if (= :expr context) :return context)
     :name name
     :try try
     :catch catch
     :finally finally
     :try-statements (:statements try)
     :try-ret (:ret try)
     :catch-statements (:statements catch)
     :catch-ret (:ret catch)
     :finally-statements (:statements finally)
     :finally-ret (:ret finally)}))


(defmethod parse :let
  [{:keys [bindings statements ret env loop]}]
  {:node :let
   :context (:context env)
   :env env
   :bindings bindings  ;; parse
   :loop loop
   :statements statements
   :ret ret})

(defmethod parse :recur
  [{:keys [frame exprs env]}]
  {:node recur
   :context (:context env)
   :env env
   :temps (vec (take (count exprs) (repeatedly gensym)))
   :names (:names frame)
   :exprs exprs})

(defmethod parse :new
  [{:keys [ctor args env]}]
  {:node :new
   :args args
   :context (:context env)
   :ctor ctor
   :env env})

(defmethod parse :letfn
  [{:keys [bindings statements ret env]}]
  {:node :letfn
   :context (:context env)
   :env env
   :bindings bindings
   :statements statements
   :ret ret})

(defmethod parse :set!
  [{:keys [target val env]}]
  {:node :set!
   :context (:context env)
   :env env
   :target target
   :val val})

(defmethod parse :ns
  [{:keys [name requires uses requires-macros env]}]
  {:node :ns
   :context (:context env)
   :env env
   :name name
   :requires requires
   :uses uses
   :requires-macros requires-macros
   :special-namespaces #{'cljv.core}})

(defmethod parse :deftype*
  [{:keys [t fields pmasks]}]
  {:node :deftype*
   :context (:context env)
   :env env
   :t t
   :fields fields
   :pmasks pmasks})

(defmethod parse :defrecord*
  [{:keys [t fields pmasks]}]
  {:node :defrecord*
   :context (:context env)
   :env env
   :t t
   :fields fields
   :extra-fields = '[__meta __extmap]
   :pmasks pmasks})

(defmethod parse :dot
  [{:keys [target field method args env]}]
  {:node :dot
   :context (:context env)
   :env env
   :target target
   :field field
   :method method
   :args args})

(defmethod parse :fn [_])
(defmethod parse :invoke [_])


(defmethod parse :java
  [{:keys [env code segs args]}]
  {:node :java
   :context (:context env)
   :env env
   :code code
   :segs segs
   :args args})

(defmethod parse :default [_])

