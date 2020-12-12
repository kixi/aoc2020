(ns aoc.d11
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as c]))

(def demo
  (str/split-lines
   "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))

(def demo (cmn/slurp-lines "d11.txt"))

(def wa0
  (->> demo
       (mapv #(into [] %))
       (into []))
  )

(def directions
  (for [x [-1 0 1] y [-1 0 1] :when (not= x y 0)]
    [x y]))

(defn v+ [v0 v1]
  (mapv + v0 v1))

(defn log [x]
  (prn x)
  x)

(defn rc [pv]
  [(second pv) (first pv)])

(defn find-move
  [wa pv dir]
  (let [rv (first  (drop-while #(= \. (get-in wa (rc %))) (rest (iterate (partial  v+ dir) pv))))] ;
    (when (get-in wa (rc rv)) rv)))

(defn neighbours [waiting-area px py]
  (let [height (count waiting-area)
        width (count (first waiting-area))
        moves (map (partial v+ [px py]) directions)]
    (->>
     moves
     (filter (fn [[x y]] (and (<= 0 x (dec width))
                              (<= 0 y (dec height))))

             )
     (map #(get-in waiting-area [(second  %) (first %)])))))

(defn neighbours [waiting-area px py]
  ( let [height (count waiting-area)
         width (count (first waiting-area))
         moves (keep #(find-move waiting-area [px py] %)   directions)]
   (->>
    moves
    (filter identity)
    (map #(get-in waiting-area [(second  %) (first %)])))))


(defn calc-seat [waiting-area x y]
  (let [seat (get-in waiting-area [y x])]
    (if (= seat \.)
      \.
      (let [n (neighbours waiting-area x y)]
        (cond
          (and (= seat \L) (not-any? #(= % \#) n))
          \#

          (and (= seat \#) (>=  (get (frequencies n) \# 0) 5))
          \L

          :else
          seat
          ))))
  )

(defn step [wa]
(reduce (fn [wa* [x y]]
          (assoc-in wa* [y x] (calc-seat wa x y))
          )
        wa
        (for [x (range 0 (count (first wa)))
              y (range 0 (count wa))]
          [x y])))

(def wa1 (log  (step wa0)))
(def wa2 (log  (step wa1)))

(neighbours wa1 0 2)
(calc-seat wa1 3 0)
(defn log [x] (clojure.pprint/pprint x) x)
(->> wa0
     (iterate step)
     (partition 2 1)
     (drop-while (fn [p] (not= (first p) (second p))))
     first
     first
     flatten
     (filter #(= % \#))
     count)
