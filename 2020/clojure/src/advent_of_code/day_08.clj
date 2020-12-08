(ns advent-of-code.day-08
  (:require [clojure.string :as str]))

(defn part-1
  "Day 08 Part 1"
  [input]
  input)

(defn part-2
  "Day 08 Part 2"
  [input]
  input)

(def sample-input
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def input
  (-> (slurp "resources/day-08.txt")
      (str/split #"\n")))

(defn process-instruction
  [{:keys [acc pos] :as return} [action dir-value]]
  (let [value (Integer/parseInt dir-value)]
    (cond-> return
      (= "acc" action)
      (-> (assoc :acc (+ acc value))
          (assoc :pos (inc pos)))

      (= "jmp" action)
      (assoc :pos (+ pos value))

      (= "nop" action)
      (assoc :pos (inc pos))

      :always
      (update :visited #(conj % pos)))))

(defn run-ops
  [instructions]
  (loop [{:keys [acc pos visited] :as return}
         {:acc 0
          :pos 0
          :visited #{}}]
    (if (visited pos)
      acc
      (let [next (nth instructions pos)]
        (recur (process-instruction return next))))))

(comment
  (->> input
       (map #(str/split % #" "))
       (run-ops))
  (first)
  (#(process-instruction {:acc 0
                          :pos 0
                          :visited #{}}
                         %)))
