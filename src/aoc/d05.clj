(ns aoc.d05
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def letters {\F 0 \B 1 \L 0 \R 1})

(def input (cmn/slurp-lines "d05.txt"))

(defn bin->dec [x]
  (reduce (fn [s x] (+ (* 2 s) x))
          0
          x))

(defn seat-id [seat]
 (bin->dec (map letters seat)))

(def seat-ids (map seat-id input))

;;;; star 1
(apply max seat-ids)

;; star 2
(->> (partition 2 1 (sort seat-ids))
     (some (fn [[x0 x1]]
             (when (= 2 (- x1 x0))
               (inc x0)))))
