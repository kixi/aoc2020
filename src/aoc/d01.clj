(ns aoc.d01
  (:require [aoc.cmn :as c]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (c/slurp-numbers "d01.txt"))

;; star1
(->
 (for [x input
       y input
       :when (= (+ x y) 2020)]
   (* x y))
 first)


;; star2
(->
 (for [x input
       y input
       z input
       :when (= (+ x y z) 2020)]
   (* x y z))
 first)
