(ns aoc.cmn
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn slurp-lines
  [resource-name]
  (->
   resource-name
   io/resource
   slurp
   s/split-lines))

(defn slurp-numbers [resource-name]
  (->>
   (slurp-lines resource-name)
   (map read-string)))
