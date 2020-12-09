(ns advent-of-code.day-01
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn- check-sublists-pt1
  [target [x & nums]]
  (if-let [match (some #(when (= (- target x) %)
                          %) nums)]
    [x match]
    (check-sublists-pt1 target nums)))

(defn part-1
  "Day 01 Part 1"
  [input]
  (let [nums (->> (str/split input #"\n")
                  (map edn/read-string))
        target 2020]
    (apply * (check-sublists-pt1 target nums))))

(defn- check-sublists-pt2
  [target nums]
  (loop [target target
         [x y & nums] nums]
    (print x " " y "\n")
    (if-let [match (and
                   ;; ensure that the target is not smaller than sum of first 2 nums
                   ;; (>= target (+ x y))
                    (some #(when (= (- target x y) %)
                             %)
                          nums))]
      [x y match]
      (recur target (cons y nums)))))

(defn part-2
  "Day 01 Part 2"
  [input]
  (let [nums (->> (str/split input #"\n")
                  (map edn/read-string))
        target 2020]
    (->>
     (for [a nums
           b nums
           c nums
           :when (= target (+ a b c))]
       #{a b c})
     first
     (apply *))))

(comment
  (part-2 (slurp "resources/day-01.txt")))
