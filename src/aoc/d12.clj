(ns aoc.d12
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as c]))

(def d (str/split-lines "F10
N3
F7
R90
F11"))


(def directions
  {:S [0 -1]
   :E [1 0]
   :W [-1 0]
   :N [0 1]})

(def turns
  {:R (fn [[x y]] [y (- x)] )
   :L (fn [[x y]] [(- y) x])})

(def instructions
  (->> (cmn/slurp-lines "d12.txt")
       (map #(re-find #"(\w)(\d+)" %))
       (map (fn [[_ d s]]
              [(keyword  d) (read-string s)]))))

(defn move-dir [pos dir arg]
  (prn pos dir arg)
  (mapv (fn [p d] (+ p (* d arg))) pos dir ))

(defn change-dir [dir f arg]
  (first
   (drop  (/ arg 90)
          (iterate (turns f) dir))) )

(defn move [{:keys [dir pos] :as position} [inst arg]]
  (cond
    (directions inst)
    (update position :pos move-dir (directions inst) arg)

    (turns inst)
    (update position :pos change-dir inst arg)
    :else
    (update position :ship move-dir pos arg)))


(def pos
  {:dir [1 0]
   :pos [10 1]
   :ship [0 0]})
(->>
 (reduce move pos instructions)
 (:ship )
 (apply + ))
