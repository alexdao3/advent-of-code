(ns advent-of-code.day-03
  (:require [clojure.string :as str]))

(defn num-maps
  [x-steps input]
  (let [final-x-coord (* x-steps (count input))]
    (inc (quot final-x-coord (count (first input))))))

(defn check-trees
  [x-steps y-steps xs]
  (loop [[y & rest] (partition y-steps xs)
         trees 0
         x-index 0]
    (println (count y))
    (if (not= y-steps (count y))
      trees
      (recur rest
             (if (= \# (first (drop x-index (first y))))
               (inc trees)
               trees)
             (+ x-index x-steps)))))
             
(defn build-map
  [input x-steps]
  (let [num-maps (num-maps x-steps input)]
    (->> input
         (map #(repeat num-maps %))
         (map #(str/join "" %)))))
      
(defn solve-trees
  [input x-steps y-steps]
  (->> (build-map input x-steps)
       (check-trees x-steps y-steps)))

(defn part-1
  "Day 03 Part 1"
  [input]
  (let [input (-> (slurp input) str/split-lines)]
    (solve-trees input 1 2)))

(defn part-2
  "Day 03 Part 2"
  [input]
  (let [input (-> (slurp input) str/split-lines)]
    (->>
     (map (fn [[x-steps y-steps]]
            (solve-trees input x-steps y-steps))
          [[1 1] [3 1] [5 1] [7 1] [1 2]])
     (apply *))))
    
    
  

(def input
  (-> (slurp "resources/day-03.txt")
      (str/split-lines)))


(comment
  ;; part 1
  (->> input
       (map #(repeat num-maps %))
       (map #(str/join "" %))
       (check-trees))
  (part-1 "resources/day-03.txt")
  (part-2 "resources/day-03.txt")

  [[1 1] [3 1] [5 1] [7 1] [1 2]]


  num-maps)

         
