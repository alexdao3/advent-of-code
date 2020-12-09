(ns advent-of-code.day-09
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn part-1
  "Day 09 Part 1"
  [input]
  input)

(defn part-2
  "Day 09 Part 2"
  [input]
  input)

(defn target-valid?
  [nums target]
  (for [a nums
        b nums
        :when (= target (+ a b))]
    [a b]))

(def example-input
  (->
   "35
  20
  15
  25
  47
  40
  62
  55
  65
  95
  102
  117
  150
  182
  127
  219
  299
  277
  309
  576"
   (str/split #"\n")))

(comment
  ;; part 1 --  10884537
  (let [nums (->> (str/split (slurp "resources/day-09.txt") #"\n")

                  (map str/trim)
                  (map #(edn/read-string %)))
        window 25
        lists (map #(take (inc window) (drop % nums)) (range (count nums)))]
    (loop [[part & rest] lists]
      (let [next (last part)
            window-to-check (take window part)]
        (if (seq (target-valid? window-to-check next))
          (recur rest)))))

  ;; part 2 -- 1261309
  (let [nums (->> (str/split (slurp "resources/day-09.txt") #"\n")
                  (map str/trim)
                  (map #(edn/read-string %)))
        target 10884537
        start-idx 0
        end-idx 1]
    (loop [min-idx nil                  ; min idx of longest contiguous
           max-idx nil                  ; max idx of longest contiguous
           start-idx start-idx          ; starting pointer
           end-idx end-idx             ; ending pointer
           lil-sum 0]
      (let [num-elems (- end-idx start-idx)
            rest-of-elems (drop start-idx nums)
            sub-list (take num-elems rest-of-elems)
            sum (apply + sub-list)]
        (cond
          ;; terminate once we've reached end of list
          (>= end-idx (count nums))
          lil-sum

          ;; if sum === target, check if the contiguous range is longer than previous contiguous range and set the min-max indices accordingly
          (=
           target
           sum)
          (if (or (nil? min-idx) (> (- end-idx start-idx) (- min-idx max-idx)))
            (recur start-idx end-idx start-idx (inc end-idx) (+ (apply max sub-list) (apply min sub-list)))
            (recur min-idx max-idx start-idx (inc end-idx) lil-sum))

          ;; if sum is greater, then we need to move starting pointer up a slot
          (> sum target)
          (recur min-idx max-idx (inc start-idx) end-idx lil-sum)

          ;; otherwise assume sum is lower, in which case we move the end pointer up a slot
          :else
          (recur min-idx max-idx start-idx (inc end-idx) lil-sum))))))
