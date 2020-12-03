(ns advent-of-code.day-02
  (:require [clojure.string :as str]))

(defn part-1
  "Day 02 Part 1"
  [input]
  (letfn [(check-valid-pw
            [entry]
            (let [[fst [letter] pw] (str/split entry #" ")
                  [min-value max-value] (map read-string (str/split fst #"-"))
                  letter-count (count (filter #(= letter %) pw))]
              (<= min-value letter-count max-value)))]
    (->> (filter check-valid-pw input)
         count)))

(defn part-2
  "Day 02 Part 2"
  [input]
  (letfn [(check-valid-pw
            [entry]
            (let [[fst [letter] pw] (str/split entry #" ")
                  pwd-indices (map (comp dec read-string) (str/split fst #"-"))
                  valid-positions (filter #(= letter (nth pw %)) pwd-indices)]
              (= 1 (count valid-positions))))]
    (->> (filter check-valid-pw input)
         count)))

(comment
  (let [[fst [letter] pw]
        (-> (slurp "resources/day-02.txt")
            (str/split #"\n")
            (first)
            (str/split #" "))
        [min-value max-value] (map read-string (str/split fst #"-"))]
    (<= min-value (count (filter #(= letter %) pw)) max-value))

  (-> (slurp "resources/day-02.txt")
      (str/split #"\n")
      (part-1)))
