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

(defn v+ [v0 v1]
  (mapv + v0 v1))
(v+ [1 2] [3 4])

(defn rotate [direction [x y]]
  (case direction
    :cw  [y (- x)]
    :ccw [(- y) x]))

(defn v* [v s]
  (mapv #(* % s) v))

(defn manhatten [x y]
  (+ (Math/abs x) (Math/abs y)))
