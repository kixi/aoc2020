(ns aoc.d15
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as c]))

(def state
  {:history
   {0 0
    3 1}
   :pos 2
   :curr 6}
  )

(def state
  {:history
   {2 0
    1 1
    10 2
    11 3
    0 4}
   :pos 5
   :curr 6}
  )

(defn next-state [state]

  (let [hist-state  (get-in state [:history (:curr state)])]
    (if (not hist-state)
      (-> state
          (assoc :curr 0)
          (update :pos inc)
          (update :history merge
                  {(:curr state)
                   (:pos state)}))
      (-> state
          (assoc :curr (- (:pos state) hist-state))
          (update :pos inc)
          (update :history merge
                  {(:curr state)
                   (:pos state)})
          ))))
(prn
 (:curr  (first  (drop (- 30000000 6) (iterate next-state state)))))
