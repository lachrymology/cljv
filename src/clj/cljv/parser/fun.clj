(ns cljv.parser.fun)

(declare really-parse-this-time)

(defn parse-fn-ast
  [{:keys [name env methods max-fixed-arity variadic recur-frames loop-lets]
    :as node}]
  (merge 
   {:node :fn
    :context (:context env)
    :env env
    :max-fixed-arity max-fixed-arity
    :variadic variadic
    :loop-locals (->> (concat (mapcat :names (filter #(and % @(:flag %)) recur-frames))
                              (mapcat :names loop-lets))
                      seq)
    :name (or name (gensym))
    :max-params (apply max-key count (map :params methods))}

   (really-parse-this-time node)))

(defmulti really-parse-this-time
  (fn [node]
    {:variadic (:variadic node)
     :max-fixed-arity (:max-fixed-arity node)
     :method-count (count (:methods node))}))

(defmethod really-parse-this-time
  {:variadic        false
   :max-fixed-arity 1
   :method-count    1}
  [{:keys [name env methods max-fixed-arity variadic recur-frames loop-lets]}]

  #_{:methods (sort-by #(-> % :params count) methods)}

  :here)

(defmethod really-parse-this-time :default [_] :yuck)

(comment
  (fn foo [n] 42)

  =>

  {:env {}
   :op :fn,
   :name foo,
   :variadic false,
   :recur-frames nil,
   :jsdoc [nil],
   :max-fixed-arity 1
   :methods
   ({:children [{:op :constant,
                 :env {},
                 :form 42}],
     :ret {:op :constant,
           :env {},
           :form 42},
     :statements nil,
     :env {},
     :variadic false,
     :params (n),
     :max-fixed-arity 1,
     :gthis nil,
     :recurs nil
     }),
   }

  (def n '{:env {}
   :op :fn,
   :name foo,
   :variadic false,
   :recur-frames nil,
   :jsdoc [nil],
   :max-fixed-arity 1
   :methods
   ({:children [{:op :constant,
                 :env {},
                 :form 42}],
     :ret {:op :constant,
           :env {},
           :form 42},
     :statements nil,
     :env {},
     :variadic false,
     :params (n),
     :max-fixed-arity 1,
     :gthis nil,
     :recurs nil
     }),
           })

  (def f (fn [node]
    {:variadic (:variadic node)
     :max-fixed-arity (:max-fixed-arity node)
     :method-count (count (:methods node))}))

  (really-parse-this-time n)
  
  (f n)
)