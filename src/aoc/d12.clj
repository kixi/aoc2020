(ns aoc.d12
  (:require [aoc.cmn :as cmn]
            [clojure.string :as str]))

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
  {:R (partial cmn/rotate :cw)
   :L (partial cmn/rotate :ccw)})

(def instructions
  (->> (cmn/slurp-lines "d12.txt")
       (map #(re-find #"(\w)(\d+)" %))
       (map (fn [[_ d s]]
              [(keyword  d) (read-string s)]))))

(defn move-dir [pos dir l]
  (cmn/v+ pos (cmn/v* dir l)))

(defn change-dir [dir f degrees]
  (->> dir
       (iterate (turns f))
       (drop  (/ degrees 90))
       first))

(defn move [{:keys [pos] :as position} [inst arg]]
  (cond
    (directions inst)
    (update position :pos move-dir (directions inst) arg)

    (turns inst)
    (update position :pos change-dir inst arg)

    :else
    (update position :ship move-dir pos arg)))

(def pos
  {:pos  [10 1]
   :ship [0 0]})

(->>
 (reduce move pos instructions)
 :ship
 (apply cmn/manhatten))
