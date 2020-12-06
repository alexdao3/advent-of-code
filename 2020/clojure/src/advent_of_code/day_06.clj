(ns advent-of-code.day-06
  (:require
   [clojure.string :as str] 
   [clojure.set :as set]))

(defn part-1
  "Day 06 Part 1"
  [input]
  input)

(defn part-2
  "Day 06 Part 2"
  [input]
  input)

;; evil
(def yes
  #{\a \b \c \x \y \z})

(def input
  (-> (slurp "resources/day-06.txt")
      (str/split #"\n\n")))

(comment
  ;; part 1
  (->> (str/split input #"\n\n") 
       (map #(str/split % #"\n"))
       (map #(apply concat %)) 
       (map set)
       (map count)
       (apply +))


  ;; part 2
  (->> (str/split input #"\n\n") 
       (map #(str/split % #"\n"))
       (map #(map set %))
       (map #(apply set/intersection %))
       (map count)
       (apply +)))
