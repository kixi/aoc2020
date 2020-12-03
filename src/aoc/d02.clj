(ns aoc.d02
  (:require
   [aoc.cmn :as c]
   [clojure.string :as s]))

(def pwd-strs (c/slurp-lines "d02.txt"))

(defn parse-policy-line
  [[p1 p2 pwd]]
  {:repetition (mapv read-string (s/split p1 #"-"))
   :letter (first p2)
   :pwd pwd})

(defn valid
  [{:keys [repetition letter pwd]}]
  (let [[lower upper] repetition]
    (<= lower
        (count (filter #(= % letter) pwd))
        upper)))

(defn count-valid-pwds
  [policy]
  (->> pwd-strs
       (map #(s/split % #" "))
       (map parse-policy-line)
       (map policy)
       (remove false?)
       (count)))

;; star1
(count-valid-pwds valid)

;;star2
(defn valid2
  [{:keys [repetition letter pwd]}]
  (->> repetition
       (filter #(= letter (.charAt pwd (dec %))))
       count
       (= 1)))

;; star2
(count-valid-pwds valid2)
