(ns cljv.compiler
  (:require [cljv.java :as java]
            [cljv.util :as util]
            [clojure.java.io :as io]))

(def ^:static +OUT-DIR+ "out")

(defn compile-root
  "Looks recursively in src-dir for .cljv files and compiles them to
   .js files. If target-dir is provided, output will go into this
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
