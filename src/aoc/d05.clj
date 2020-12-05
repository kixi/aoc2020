(ns aoc.d05
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.java.io :as io]))




(def ex-seat "FBFBBFFRLR")

(defn get-row-cols [seat]
  (let [[_ row col] (re-find #"^([FB]{7})([LR]{3})$" seat)]
    [row col]))

(get-row-cols ex-seat)

(defn bin [bs z o]
  (let [s (-> bs
              (str/replace z "0")
              (str/replace o "1"))]
    (Integer/parseInt s 2)))

(defn seat-id [seat]
  (let [[row col] (get-row-cols seat)]
    (+
       (* (bin row "F" "B") 8)
       (bin col "L" "R"))))

(seat-id ex-seat)
;; star 1
(->>
  (cmn/slurp-lines "d05.txt")
  (map seat-id)
  (apply max))

(def seats
      (->>
       (cmn/slurp-lines "d05.txt")
       (map seat-id)
       sort))
;; star 2
(->>
     (map (fn [s1 s2] [s1 s2 (- s2 s1)]) seats (rest seats))
     (filter (fn [[s1 _ d]] (= d 2)))
     (map (fn [[s1 _ _]] s1))
     first
     inc)

;; star 2 reduce
(->>
   (reduce (fn [last curr]
             (if (= (- curr last) 2)
               (reduced (inc last))
               curr))
           (first seats)
           (rest seats)))
