(ns advent-of-code.day-13
  (:require [clojure.string :as str]))

(def input
  (str/split (slurp "resources/day-13.txt") #"\n"))

(defn part-1
  "Day 13 Part 1"
  [input]
  input)

(defn part-2
  "Day 13 Part 2"
  [input]
  input)

;; (def time (Integer/parseInt (first input)))
(def bus-times (keep #(when (not= "x" %)
                        (Integer/parseInt %))
                     (str/split (second input) #",")))

(def bus-times-2 (keep #(if (not= "x" %)
                          (Integer/parseInt %)
                          %)
                       (str/split (second input) #",")))

(defn get-next-bus
  "Part 1"
  [original-time]
  (loop [time original-time]
    (let [next-bus (some #(when (= 0 (mod time %))
                            %) bus-times)]
      (if next-bus
        (* (- time original-time) next-bus)
        (recur (inc time))))))

(defn subsequent?
  [time bus-times]
  (if (reduce
       (fn [next-time next-bus]
         (if (or (= "x" next-bus) (zero? (mod next-time next-bus)))
           (inc next-time)
           (reduced false)))
       time
       bus-times)
    time
    false))

;; (subsequent? 1068781 bus-times-2)

;; (.indexOf bus-times-2 59)
(defn get-next-longest-bus-time
  [original-time longest-bus]
  (some #(when (and (not= 0 %)
                    (zero? (mod (+ original-time %) longest-bus)))
           (+ original-time %))
        (range)))

(defn get-consecutive-buses
  [original-time bus-times]
  (let [longest-bus (apply max (filter number? bus-times))
        index-max-value (.indexOf bus-times longest-bus)
        next-longest-bus (get-next-longest-bus-time original-time longest-bus)]
    next-longest-bus
    (println longest-bus next-longest-bus)
    (loop [time (- next-longest-bus index-max-value)]
      (if (subsequent? time bus-times)
        time
        (recur (+ time (- longest-bus index-max-value)))))))



;; (apply max (filter number? bus-times))


;; (get-next-bus time)
;; (get-consective-buses time bus-times-2)


;; (time (get-consecutive-buses 1068781 bus-times-2))


;; (mod 0 557)
;; (take 5 (range 0))
;; (+ 1 1)
