(ns aoc.d04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def input (slurp (io/resource "d04.txt")))

(defn parse-record [rec]
  (->>
      (str/split rec #"\s")
      (map #(str/split % #":"))
      (into {})))

(count
 (parse-record
  "ecl:brn pid:760753108 byr:1931"))

(defn parse
  [input]
  (->
   input
   (str/split #"\n\n")
   (->> (map parse-record))))

(defn valid-passport?
  [passport]
  (= 7
     (-> passport
         (dissoc "cid")
         count)))

;;star 1

(->> (parse input)
     (map valid-passport?)
     (filter identity)
     count)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-long
  [x]
  (Long/parseLong x))

(def rules
  {"byr" (fn [x]
           (and (re-matches #"^\d\d\d\d$" x)
                (<= 1920 (parse-long x) 2002)))
   "iyr" (fn [x]
           (and (re-matches #"^\d\d\d\d$" x)
                (<= 2010 (parse-long x) 2020)))
   "eyr" (fn [x]
           (and (re-matches #"^\d\d\d\d$" x)
                (<= 2020 (parse-long x) 2030)))
   "hgt" (fn [x]
           (try
             (let [[_ y u]
                   (or
                    (re-find #"^(\d\d)(in)$" x)
                    (re-find #"^(\d\d\d)(cm)$" x))]
               (case u
                 "in" (<= 59 (parse-long y) 76)
                 "cm" (<= 150 (parse-long y) 193)
                 false))
             (catch Exception _ false)))
   "hcl" (fn [x] (re-matches #"^#[0-9a-f]{6}$" x))
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" (fn [x] (re-matches #"[0-9]{9}" x))})


((get rules "hgt") "163cm")
((get rules "hcl") "#1533af")
((get rules "ecl") "grn")
((get rules "pid") "3423423434")
(re-matches #"^\d\d\d\d$" "234")
(<= 1929 (parse-long "3454"))
(parse-long "3454")


(defn valid2 [passport]
  (and (= (set (keys (dissoc passport "cid")))
          (set (keys rules)))
       (reduce (fn [a [k v]]
                 (and a
                      ((get rules k) v)))
               true
               (dissoc passport "cid"))))
;; star2
(->> (parse input)
     (map valid2)
     (filter identity)
     count)


