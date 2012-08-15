(ns cljv.parser
  (:require [cljv.java :as java]
            [clojure.string :as string]
            [cljs.compiler :as common]
            [cljs.analyzer :as ana]
            [cljv.parser.fun :as fun]))

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
   :test (parse test)
   :checked (not (or unchecked (safe-test? test)))
   :then (parse then)
   :else (parse else)
   :env env})

(defmethod parse :throw
  [{:keys [throw env]}]
  {:node :throw
   :context (:context env)
   :env env
   :throw (parse throw)})

(defmethod parse :def
  [{:keys [name init env doc export]}]
  {:node :def
   :context (:context env)
   :env env
   :name (parse name)
   :init (parse init)
   :export (parse export)
   :doc doc
   :jvdoc (:jsdoc init)})

(defmethod parse :do
  [{:keys [statements ret env]}]
  {:node :do
   :context (:context env)
   :env env
   :statements (map parse statements)
   :ret (parse ret)})

(defmethod parse :try*
  [{:keys [env try catch name finally]}]
  (assert (not= :constant (-> finally :ret :op))
          "finally block cannot contain constant")
  
  (let [context (:context env)]
    {:node :try*
     :context context
     :subcontext (if (= :expr context) :return context)
     :name (parse name)
     :try try
     :catch catch
     :finally finally
     :try-statements (map parse (:statements try))
     :try-ret (parse (:ret try))
     :catch-statements (map parse (:statements catch))
     :catch-ret (parse (:ret catch))
     :finally-statements (map parse (:statements finally))
     :finally-ret (parse (:ret finally))}))


(defmethod parse :let
  [{:keys [bindings statements ret env loop]}]
  {:node :let
   :context (:context env)
   :env env
   :bindings (map (fn [{:keys [name init]}]
                    {:name name
                     :init (parse init)})
                  bindings)
   :loop loop
   :statements (map parse statements)
   :ret (parse ret)})

(defmethod parse :recur
  [{:keys [frame exprs env]}]
  {:node :recur
   :context (:context env)
   :env env
   :temps (vec (take (count exprs) (repeatedly gensym)))
   :names (:names frame)
   :exprs (map parse exprs)})

(defmethod parse :new
  [{:keys [ctor args env]}]
  {:node :new
   :args (map parse args)
   :context (:context env)
   :ctor (parse ctor)
   :env env})

(defmethod parse :letfn
  [{:keys [bindings statements ret env]}]
  {:node :letfn
   :context (:context env)
   :env env
   :bindings (map (fn [{:keys [name init]}]
                    {:name name
                     :init (parse init)})
                  bindings)
   :statements (map parse statements)
   :ret (parse ret)})

(defmethod parse :set!
  [{:keys [target val env]}]
  {:node :set!
   :context (:context env)
   :env env
   :target (parse target)
   :val (parse val)})

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
  [{:keys [t fields pmasks env children]}]
  {:node :deftype*
   :context (:context env)
   :children (map parse children)
   :env env
   :t t
   :fields fields
   :pmasks pmasks})

(defmethod parse :defrecord*
  [{:keys [t fields pmasks env children]}]
  {:node :defrecord*
   :context (:context env)
   :children (map parse children)
   :env env
   :t t
   :fields fields
   :extra-fields '[__meta __extmap]
   :pmasks pmasks})

(defmethod parse :dot
  [{:keys [target field method args env]}]
  {:node :dot
   :context (:context env)
   :env env
   :target target
   :field field
   :method method
   :args (map parse args)})

(defmethod parse :fn
  [node]
  (fun/parse-fn-ast node))  ;; deferring to a specialist

(defn- infer-protocol-dispatch
  [{:keys [f args env]}]
  (let [info (-> f :info)
        protocol (:protocol info)
        tag (infer-tag (first args))]
    (and protocol tag
         (or ana/*cljs-static-fns*
             (:protocol-inline env))
         (or (= protocol tag)
             (when-let [ps (:protocols (ana/resolve-existing-var (dissoc env :locals) tag))]
               (ps protocol))))))

;; TODO: needs love
(defmethod parse :invoke
  [{:keys [f args env] :as expr}]
  (let [info (:info f)
        protocol (:protocol info)
        variadic? (:variadic info)
        arity (count args)
        mps (:method-params info)
        arities (map count mps)
        mfa (:max-fixed-arity info)
        proto? (infer-protocol-dispatch expr)]
    {:node :invoke
     :context (:context env)
     :env env
     :info info
     :arity arity
     :variadic? variadic?
     :method-params mps
     :fn? (and ana/*cljs-static-fns*
               (not (:dynamic info))
               (:fn-var info))
     :protocol protocol
     :proto? proto?
     :opt-not? (and (= (:name info) 'cljs.core/not)
                    (= (infer-tag (first (:args expr))) 'boolean))
     :ns (:ns info)
     :args args
     :java? (= 'js (:ns info))   ;; wrong, rethink this
     :keyword? (and (= (-> f :op) :constant)
                    (keyword? (-> f :form)))
     :dispatch (cond
                (and (not variadic?) (= (count mps) 1))
                {:f f
                 :max-fixed-arity nil
                 :variadic? false
                 :rename? false
                 :direct? true}

                (and variadic? (> arity mfa))
                {:f f
                 :max-fixed-arity mfa
                 :variadic? true
                 :rename? true
                 :direct? true}

                (and arities (some #{arity} arities))
                {:f f
                 :max-fixed-arity mfa
                 :direct? true
                 :rename? true
                 :variadic? false}

                :else
                {:f f
                 :max-fixed-arity mfa
                 :variadic? false
                 :rename? false
                 :direct? true})}))

(defmethod parse :java
  [{:keys [env code segs args]}]
  {:node :java
   :context (:context env)
   :env env
   :code code
   :segs segs
   :args args})

(defmethod parse :default [_])

