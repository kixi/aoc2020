(ns aoc.d10
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as c]))

(def demo [16 10 15 5 1 11 7 19 6 12 4])

(def demo2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn log [x]
  (prn x)
  x)

(def input (cmn/slurp-numbers "d10.txt"))

;; part 1
(->
 input
 (conj 0)
 (conj (+ 3 (apply max input)))
 sort
 (->>
  (partition 2 1)
  (map (partial apply -))
  frequencies
  vals
  (apply *)
  ))

;; part 2
(def combinations
  {1 1
   2 2
   3 4
   4 7})

(->
 input 
 (conj 0)
 (conj (+ 3 (apply max input )))
 sort
 (->>
  (partition 2 1)
  (map #(- (second %) (first %)))
  (partition-by identity)
  (remove #(= 3 (first %))
          )
  (map count)
  (map combinations)
  (apply *))
 )

