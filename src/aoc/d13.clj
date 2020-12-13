(ns aoc.d13
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as c]))
;; part 1

;; I was too lazy to implement it
(mod 939 7)
(- 59  (mod 939 59))

(def depart-time 939)
(def depart-time 1000390)

(def ids [13,41,997,23,19,29,619,37,17])
(defn  waiting-time [id]
  (- id (mod depart-time id)))

(waiting-time 59)


(map (fn [id] [id (waiting-time id)]) ids)

;; part 2

(def demo  "7,13,x,x,59,x,31,19")

(def demo "13,x,x,41,x,x,x,x,x,x,x,x,x,997,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,619,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,17" )

(def busses
  (-> demo
      (str/split #",")
      (->>  (map-indexed (fn [i n] [i n]))
            (filter #(not= "x" (second %)))
            (map (fn [[i n]] [i (Long/parseLong n)])))

      ))

;; I just brute forward find solutions for the inverse of mod
;; There is probably a much better way to do it!
(defn mod-inverse
  [n d]
  (let [q (mod n d)]
    (prn q)
    (some #(when (= (mod (* q %) d)
                    1)
             %)
          (map inc  (range)))))

;; Chinese remainder theorem
;; Solution found via reddit
;; I have no idea about number theory
(let [N (apply * (map second busses))
      tot (for [[b n] busses]
            (let [bi (mod  (- n b) n)
                  ni (/ N n)
                  xi (mod-inverse (/ N n) n)]
              (* ni bi xi)))]

  (mod  (apply + tot) N))
