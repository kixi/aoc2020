(ns aoc.d03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))


(def landmap
  (mapv vec
       (-> "d03.txt"
           io/resource
           slurp
           s/split-lines)))

(def height (count landmap))
(def width (count (first landmap)))

(defn square [x y]
  (let [x* (mod x width)]
    (get-in landmap [y x*])))

(defn move [pos slope]
  (mapv  + pos slope))

(defn path
  [slope]
 (loop [[x y] [0 0]
        seen []]
   (if (< y height)
     (recur (move [x y] slope)
            (conj seen (square x y)))
     seen)))


(def slopes [[1 1]
             [3 1]
             [5 1]
             [7 1]
             [1 2]])

(defn tree-count
  [p]
  (count
   (filter #(= \# %) p)))

;; star1
(tree-count (path [3 1]))

;; star2
(->> slopes
     (map path)
     (map tree-count)
     (apply *))
