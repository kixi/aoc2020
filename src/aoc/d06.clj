(ns aoc.d06
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def input
  (->
   (slurp (io/resource "d06.txt"))
   (str/split #"\n\n")))

;; star1
(->>
 input
 (map #(re-seq #"\S" %))
 (map set)
 (map count)
 (apply +))

;; star2
(def groups (map str/split-lines input))

(defn count-everyone-yes [group]
  (->> group
       (map set)
       (apply set/intersection)
       count))

(->>
 groups
 (map count-everyone-yes)
 (apply +))
