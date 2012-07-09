(ns cljv.util
  (:require [clojure.string :as string]))

(defn files-in
  "Return a sequence of all files in the given directory with a given extension
   and not contained in an exclusion set."
  [dir ext excludes]
  (filter #(let [name (.getName ^java.io.File %)]
             (and (.endsWith name ext)
                  (not= \. (first name))
                  (not (contains? excludes name))))
          (file-seq dir)))

(defn mkdirs
  "Create all parent directories for the passed file."
  [^java.io.File f]
  (.mkdirs (.getParentFile (.getCanonicalFile f))))

(defn- path-seq
  [file-str]
  (->> java.io.File/separator
       java.util.regex.Pattern/quote
       re-pattern
       (string/split file-str)))

(defn- to-path
  ([parts]
     (to-path parts java.io.File/separator))
  ([parts sep]
     (apply str (interpose sep parts))))

(defn rename-as
  "Change the file extension from .cljs to .js. Takes a File or a
  String. Always returns a String."
  [file-str name-re replacement]
  (clojure.string/replace file-str name-re replacement))

(defn to-target-file
  "Given the source root directory, the output target directory and
  file under the source root, produce the target file."
  [^java.io.File dir ^String target ^java.io.File file rename-fn]
  (let [dir-path (path-seq (.getAbsolutePath dir))
        file-path (path-seq (.getAbsolutePath file))
        relative-path (drop (count dir-path) file-path)
        parents (butlast relative-path)
        parent-file (java.io.File. ^String (to-path (cons target parents)))]
    (java.io.File. parent-file ^String (rename-fn (last relative-path)))))
