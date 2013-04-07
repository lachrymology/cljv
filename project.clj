(defproject cljv "0.0.1-SNAPSHOT"
  :description "Experiments in compiling Clojure to Java."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1443"]
                 [org.clojure/core.unify "0.5.3"]
                 [cljv/clj-soy "0.3.0"]
                 [org.apache.velocity/velocity "1.7"]]
  :dev-dependencies [[jline "0.9.94"]
                     [lein-marginalia "0.7.1"]]
  :plugins [[lein-swank "1.4.4"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :source-paths ["src/clj"]
  :test-paths   ["src/test"])
