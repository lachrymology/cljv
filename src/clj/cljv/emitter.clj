(ns cljv.emitter
  (:refer-clojure :exclude (munge))
  (:require [cljv.java :as java]
            [clojure.string :as string])
  (:use cljv.emitter.constants))

(defmulti emit :op)

(defn- munge
  ([s] (munge s java/reserved))
  ([s reserved]
     (let [ss (string/replace (str s) #"\/(.)" ".$1") ; Division is special
           ss (apply str (map #(if (reserved %) (str % "$") %)
                              (string/split ss #"(?<=\.)|(?=\.)")))
           ms (clojure.lang.Compiler/munge ss)]
       (if (symbol? s)
         (symbol ms)
         ms))))

(defn- comma-sep [xs]
  (interpose "," xs))

(defn- escape-char [^Character c]
  (let [cp (.hashCode c)]
    (case cp
                                        ; Handle printable escapes before ASCII
      34 "\\\""
      92 "\\\\"
                                        ; Handle non-printable escapes
      8 "\\b"
      12 "\\f"
      10 "\\n"
      13 "\\r"
      9 "\\t"
      (if (< 31 cp 127)
        c ; Print simple ASCII characters
        (format "\\u%04X" cp))))) ; Any other character is Unicode

(defn- escape-string [^CharSequence s]
  (let [sb (StringBuilder. (count s))]
    (doseq [c s]
      (.append sb (escape-char c)))
    (.toString sb)))

(defn- wrap-in-double-quotes [x]
  (str \" x \"))

;; emission

(defn emits [& xs]
  (doseq [x xs]
    (cond
     (nil? x) nil
     (map? x) (emit x)
     (seq? x) (apply emits x)
     (fn? x)  (x)
     :else (do
             (let [s (print-str x)]
               (when *position*
                 (swap! *position* (fn [[line column]]
                                     [line (+ column (count s))])))
               (print s)))))
  nil)

(defn ^String emit-str [expr]
  (with-out-str (emit expr)))

(defn emitln [& xs]
  (apply emits xs)
  (println)
  (when *position*
    (swap! *position* (fn [[line column]]
                        [(inc line) 0])))
  nil)

(defmethod emit :no-op [_])
(defmethod emit :var [_])
(defmethod emit :meta [_])
(defmethod emit :map [_])
(defmethod emit :vector [_])
(defmethod emit :set [_])
(defmethod emit :constant [_])
(defmethod emit :if [_])
(defmethod emit :throw [_])
(defmethod emit :def [_])
(defmethod emit :fn [_])
(defmethod emit :do [_])
(defmethod emit :try* [_])
(defmethod emit :let [_])
(defmethod emit :recur [_])
(defmethod emit :letfn [_])
(defmethod emit :invoke [_])
(defmethod emit :new [_])
(defmethod emit :set! [_])
(defmethod emit :ns [_])
(defmethod emit :deftype* [_])
(defmethod emit :defrecord* [_])
(defmethod emit :dot [_])
(defmethod emit :java [_])
(defmethod emit :default [_])

