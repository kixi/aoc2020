(ns aoc.d09
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as c]))

(def demo
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 56])

(def input (-> (cmn/slurp-numbers "d09.txt")))

(defn valid? [xs]
  (let [[preamble test]
        (split-at (dec (count xs)) xs)
        s (set (map (fn [xs] (apply + xs)) (c/combinations preamble 2)))]
    (if-not (s (first test))
      (first test))))



(first (keep valid? (partition 26 1 input)))


(defn add-range [sum xs]
  (reduce (fn [s x]
            (let [r (apply + x s)]
              (cond
                (= r sum)
                (do (prn "found" s x)
                    (reduced (conj s x)))
                (> r sum)
                (reduced false)
                :else
                (conj s x))))
          []
          xs))


(loop [xs input]
  (let [r (add-range 1124361034 xs)]
    (cond
      (not (seq xs)) r
      (= r false) (recur (drop 1 xs))
      (= (count xs) (count r)) (recur (drop 1 xs))
      :else r)))

(def f *1)
(+ (apply min f) (apply max f));; => 129444555
