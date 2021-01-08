(ns advent-of-code.day-15
  (:require [clojure.string :as str]))

(defn part-1
  "Day 15 Part 1"
  [input]
  input)

(defn part-2
  "Day 15 Part 2"
  [input]
  input)

(def starting-numbers [0 3 6])

(defn solve-pt1
  [starting-numbers nth-number]
  (let [starting-last-seen (into {} (map-indexed (fn [index item] [item [index]]) starting-numbers))]
    (loop [index (count starting-numbers)
           last-value (last starting-numbers)
           last-seen starting-last-seen]
      (let [value-last-seen (get last-seen last-value)
            next-value (cond (nil? value-last-seen)
                             0

                             (< (count value-last-seen) 2)
                             0

                             :else
                             (Math/abs (apply - value-last-seen)))
            last-seen (update last-seen
                              next-value
                              (fnil
                               (fn [last-seen-value]
                                 (if (= 2 (count last-seen-value))
                                   (conj (vec (drop 1 last-seen-value)) index)
                                   (conj last-seen-value index)))
                               []))]
        (if (= (dec nth-number) index)
          next-value
          (recur (inc index) next-value last-seen))))))

