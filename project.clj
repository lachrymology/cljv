(defproject cljv "0.0.1-SNAPSHOT"
  :description "Experiments in compiling Clojure to Java."
  :dependencies [[org.clojure/clojure "1.5.0-master-SNAPSHOT"]
                 [org.clojure/clojurescript "0.0-1443"]
                 [org.clojure/core.unify "0.5.3"]
                 [cljv/clj-soy "0.3.0"]]
  :dev-dependencies [[jline "0.9.94"]
                     [swank-clojure "1.4.2"]
                     [lein-marginalia "0.7.1"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :source-path "src/clj"
  :test-path   "src/test")
