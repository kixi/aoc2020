(ns aoc.d08
  (:require [clojure.string :as str]
            [aoc.cmn :as cmn]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def code
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def inp
  (->>
   code
   (str/split-lines)
   (mapv #(str/split % #"\s"))))

(def input (cmn/slurp-lines "d08.txt"))

(def inp
  (->> input
   (mapv #(str/split % #"\s"))))

(defn create-prog
  [code]
  {:ip 0
   :prog code
   :accumulator 0
   :log []})

(defmulti exec-cmd (fn [ctx cmd args]
                     cmd))

(defmethod exec-cmd "acc" [ctx cmd args]
  (->
     ctx
     (update :accumulator + (read-string args))
     (update :ip inc)))

(defmethod exec-cmd "jmp" [ctx cmd args]
  (->
   ctx
   (update :ip + (read-string args))))

(defmethod exec-cmd "nop" [ctx cmd args]
  (->
   ctx
   (update :ip inc)))

(exec-cmd (create-prog []) "acc" "+3")
;; (exec-cmd (create-prog []) "jmp" "+3")

(defn step
  [{:keys [prog ip log] :as ctx}]
  (let [[cmd args] (nth prog ip)
        ctx* (exec-cmd ctx cmd args)
        ctx2 (update ctx* :log conj ip)]
    ctx2))

(step (create-prog inp))

(defn prog-loop
  [{:keys [prog ip log] :as ctx}]
  (cond
    (contains? (set log) ip)
    (assoc ctx :state :endless-loop)

    (>= ip (count prog))
    (assoc ctx :state :finished)

    :else
    (recur (step ctx))))

;; star 1
(prn (prog-loop (create-prog inp)))

;; star 2
(def ctx (create-prog inp))


(defn get-progs [prog]
  (->>
   (for [i (range (count prog))]
     (let [[op args] (nth prog i)]
       (case op
         "nop" (assoc prog i ["jmp" args])
         "jmp" (assoc prog i ["nop" args])
         nil)))
   (remove nil?)))


(some (fn [prog]
        (let [ctx (create-prog prog)
              r (prog-loop ctx)]
          (when (= (:state r) :finished)
            r)))
  (get-progs (:prog ctx)))
