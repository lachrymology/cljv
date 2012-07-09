(ns cljv.compiler
  (:require [cljv.java :as java]
            [cljv.emitter :as emitter]
            [cljv.util :as util] 
            [cljs.tagged-literals :as tags]
            [cljs.analyzer :as ana] 
            [clojure.java.io :as io]))

(def ^:static +OUT-DIR+ "out")
(def ^:dynamic *position* nil)

;; from cljs
(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (clojure.lang.LineNumberingPushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (read rdr nil nil)]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(defmacro with-core-cljv
  "Ensure that core.cljs has been loaded."
  [& body]
  `(do (when-not (:defs (get @ana/namespaces 'cljs.core))  ;;
         (ana/analyze-file "cljs/core.cljv"))
       ~@body))

(defn requires-compilation?
  "Return true if the src file requires compilation."
  [^java.io.File src ^java.io.File dest]
  (or (not (.exists dest))
      (> (.lastModified src) (.lastModified dest))))

(defn compile-file* [src dest]
  (with-core-cljv
    (with-open [out ^java.io.Writer (io/make-writer dest {})]
      (binding [*out* out
                ana/*cljs-ns* 'cljs.user  ;;
                ana/*cljs-file* (.getPath ^java.io.File src) ;;
                *data-readers* tags/*cljs-data-readers* ;;
                *position* (atom [0 0])]
        (loop [forms (forms-seq src)  ;;
               ns-name nil
               deps nil]
          (if (seq forms)
            (let [env (ana/empty-env) ;;
                  ast (ana/analyze env (first forms))] ;;
              (do (emitter/emit ast)  ;;
                  (if (= (:op ast) :ns)
                    (recur (rest forms) (:name ast) (merge (:uses ast) (:requires ast)))
                    (recur (rest forms) ns-name deps))))
            {:ns (or ns-name 'cljs.user) ;;
             :provides [ns-name]
             :requires (if (= ns-name 'cljs.core) (set (vals deps)) (conj (set (vals deps)) 'cljs.core))  ;;
             :file dest}))))))

(defn compile-file
  "Compiles src to a file of the same name, but with a .java extension,
   in the src file's directory.

   With dest argument, write file to provided location. If the dest
   argument is a file outside the source tree, missing parent
   directories will be created. The src file will only be compiled if
   the dest file has an older modification time.

   Both src and dest may be either a String or a File.

   Returns a map containing {:ns .. :provides .. :requires .. :file ..}.
   If the file was not compiled returns only {:file ...}"
  ([src]
     (let [dest (java/rename-to-java src)]
       (compile-file src dest)))
  ([src dest]
     (let [src-file (io/file src)
           dest-file (io/file dest)]
       (if (.exists src-file)
         (if (requires-compilation? src-file dest-file)
           (do (util/mkdirs dest-file)
               (compile-file* src-file dest-file))
           {:file dest-file})
                  (throw (java.io.FileNotFoundException. (str "The file " src " does not exist.")))))))

(defn compile-root
  "Looks recursively in src-dir for .cljv files and compiles them to
   .java files. If target-dir is provided, output will go into this
   directory mirroring the source directory structure. Returns a list
   of maps containing information about each file which was compiled
   in dependency order."
  ([src-dir]
     (compile-root src-dir +OUT-DIR+))
  ([src-dir target-dir]
     (let [src-dir-file (io/file src-dir)]
       (loop [cljv-files (util/files-in src-dir-file ".cljv" #{})
              output-files []]
         (if (seq cljv-files)
           (let [cljv-file (first cljv-files)
                 output-file ^java.io.File (util/to-target-file src-dir-file target-dir cljv-file java/rename-to-java)
                 ns-info (compile-file cljv-file output-file)] ;;
             (recur (rest cljv-files)
                    (conj output-files (assoc ns-info :cljv.java/file-name (.getPath output-file)))))
           output-files)))))
