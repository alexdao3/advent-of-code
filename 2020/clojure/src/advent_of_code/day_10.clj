(ns advent-of-code.day-10
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn part-1
  "Day 10 Part 1"
  [input]
  input)

(defn part-2
  "Day 10 Part 2"
  [input]
  input)

(def example-input
  (-> (slurp "resources/day-10.txt")
      (str/split #"\n")))

(comment
  ;; pt 1
  (let [nums (->> (map edn/read-string example-input)
                  sort
                  vec)
        [first & rest] (concat [0] nums [(+ 3 (last nums))])]
    (->>
     (reduce (fn [{:keys [acc prev]} next]
               {:acc (update acc (- next prev) (fnil inc 0))
                :prev next})
             {:acc {}
              :prev first}
             rest)
     :acc
     (map second)
     (apply *))))
     ;; (filter (comp #(> 0 %) first))
