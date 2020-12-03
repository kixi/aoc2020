(ns aoc.d02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(def pwd-strs
  (->
   "d02.txt"
   io/resource
   slurp
   s/split-lines))



(defn parse-policy-line
  [[p1 p2 pwd]]
  {:repetition (mapv read-string (s/split p1 #"-"))
   :letter (first p2)
   :pwd pwd})

(defn valid
  [{:keys [repetition letter pwd]}]
  (let [[f t] repetition]
    (<= f
        (count (filter #(= % letter) pwd))
        t)))
;;star1
(->> pwd-strs
  (map #(s/split % #" "))
  (map parse-policy-line)
  (map valid)
  (remove false?)
  (count))

;;star2
(defn valid2
  [{:keys [repetition letter pwd]}]
  (let [[f t] repetition
        r1 (= letter (.charAt pwd (dec f)))
        r2 (= letter (.charAt pwd (dec t)))]
    (and (or r1 r2)
         (not (and r1 r2)))))

(->> pwd-strs
  (map #(s/split % #" "))
  (map parse-policy-line)
  (map valid2)
  (remove false?)
  (count))
