(ns aoc.d14
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as c]))

(def demo
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")
(def demo (slurp (io/resource "d14.txt")))
(def p       #"(\w+)(\[(\d+)\])? = (\w+)")
(def pattern #"(\w+)(\[(\d+)\])? = (.*)")
(re-find p "mem[9] = xx")
(def prog (->> demo
               str/split-lines
               (map #(re-find p  %))
               (map (fn [[ _ c _ & args]] [(keyword c) (vec (filter some? args))]))))

(defn mask-digit [m d]
  (if (= m \X) d m))

(mask-digit \1 \0)
(defn log [x]
                                        ;  (prn x)
  x)
(defn apply-mask [mask val]
  (let [bin-val (cl-format nil "~36,'0',B" val)]
    ;;  (prn mask val bin-val)
    (log
     (Long/parseLong  (apply str (map mask-digit mask bin-val)) 2))))

(defn exec-instruction [state [inst args]]
  (case inst
    :mask (assoc state :mask (first args))
    :mem  (assoc-in state [:mem  (first args)] (apply-mask (:mask state) (Long/parseLong (second args))))))

(let [ state
      (reduce exec-instruction
              {:mask "xxx"}
              prog)]
  (apply + (vals (:mem state))))
;; => 6513443633260

(defn mask-digit2 [m d]
  (case m
    \X \X
    \1 \1
    \0 d))

(defn apply-mask2 [mask addr]
  (let [bin-val (cl-format nil "~36,'0',B" addr)]
                                        ;(prn mask addr bin-val)
    (log
     (apply str (map mask-digit2 mask bin-val)))))

(defn repl-x-mask [mask n prec]
  (let [bin-val (cl-format nil (str "~" prec ",'0',B") n)]
                                        ;(prn bin-val)
    (loop [bv bin-val res mask]
      (if (not (empty? bv))
        (recur
         (apply str  (rest bv))
         (str/replace-first res "X" (str  (first  bv)) ))
        res))))

(defn create-addresses [mask]
  (let [cx (count (filter #(= \X %) mask))
        reps (long (Math/pow 2 cx))]
    (map #(repl-x-mask %1 %2 cx) (repeat reps  mask) (range)) ))

(create-addresses "000000000000000000000000000000X1001X")

(defn exec-instruction2 [state [inst args]]
  (prn "inst" inst args)
  (case inst
    :mask (assoc state :mask (first args))
    :mem  (let [masked-addr (apply-mask2 (:mask state) (Long/parseLong (first args)))
                addrs (create-addresses masked-addr)
                upd (into {} (map (fn [k v] [k v]) addrs (repeat (Long/parseLong (second args)))))]
            (update state :mem merge upd))))


(let [ state
      (reduce exec-instruction2
              {:mask "xxx"}
              prog)]
  (apply + (vals (:mem state))))
