(ns aoc.cmn
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))


(defn slurp-numbers [resource-name]
  (->>
   (io/resource resource-name)
   slurp
   s/split-lines
   (map read-string)))
