(ns cljv.java
  (:require [cljv.util :as util]))

(def rename-to-java #(util/rename-as % #"\.cljv$" ".java"))
