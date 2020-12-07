(ns aoc.d07
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def input (cmn/slurp-lines "d07.txt"))

(def demo
  (str/split-lines
   "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."))

(def inp demo)

(defn parse-line
  [line]
  (let [[bag & content]
        (str/split line #"\s?(contain|,)\s")
        c2 (keep (fn [s]
                  (let [[_ n c] (re-find #"(\d)+\s(\w+\s\w+)" s)]
                    (when n
                      [(Integer/parseInt n) c])))
                content)]
    [(re-find #"\w+\s\w+" bag) c2]))

(defn find-parents [tr bag]
  (->> tr
   (filter (fn [[par ch]]
             (some (fn [[n c]] (= c bag)) ch)))
   (keep first)))

(defn find-roots [tr]
  (loop [bags ["shiny gold"] result []]
    (let [parents (mapcat (partial find-parents tr) bags) ]
      (if (seq parents)
        (recur parents (concat result parents))
        result))))


(->> input
     (map  parse-line)
     find-roots
     set
     count)

(def bag-tree (into {} (map parse-line input)))

(defn count-bags
  [tree bag]
  (let [children (get tree bag)]
    (apply +
           (for [[n b] children]
             (do
               (+ n (* n (count-bags tree b))))))))

(count-bags bag-tree "shiny gold")
