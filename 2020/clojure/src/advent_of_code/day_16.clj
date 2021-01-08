(ns advent-of-code.day-16
  (:require [clojure.string :as str]))

;; (def input (-> (slurp "resources/day-16.txt")
;;                (str/split #"\n\n")))

;; (def rules (first input))
;; (def my-ticket-section (first (drop 1 input)))
;; (def nearby-tickets-section (first (drop 2 input)))

;; (def nearby-tickets (->> (str/split nearby-tickets-section #"\n")
;;                          rest
;;                          (map #(str/split % #","))
;;                          (map (fn [the-vec] (map #(Integer/parseInt %) the-vec)))))

;; (defn part-1
;;   "Day 16 Part 1"
;;   [input]
;;   (let [[rules _my-ticket-section nearby-tickets-section] (str/split input #"\n\n")
;;         nearby-tickets (->> (str/split nearby-tickets-section #"\n")
;;                             rest
;;                             (map #(str/split % #","))
;;                             (map (fn [the-vec] (map #(Integer/parseInt %) the-vec))))
;;         ranges (->> (re-seq #"\d+" rules)
;;                     (map #(Integer/parseInt %))
;;                     (partition-all 2))]
;;     (->> (apply concat nearby-tickets)
;;          (remove (fn [num] (some (fn [[min max]] (<= min num max)) ranges)))
;;          (apply +))))

;; (defn part-2
;;   "Day 16 Part 2"
;;   [input]
;;   input)

;; (comment

;;   (part-1 (slurp "resources/day-16.txt"))

;;   (def ranges
;;     (->> (re-seq #"\d+" rules)
;;          (map #(Integer/parseInt %))
;;          (partition-all 2)))

;;   (->> (str/split-lines rules)
;;        (map (juxt (comp first (partial re-seq #"\w+ +\w+:")) (partial re-seq #"\d+"))))
;;   (def per-lineranges
;;     (->> (str/split-lines rules)
;;          (map (juxt (comp first (partial re-seq #"\w+ +\w+:")) (partial re-seq #"\d+")))
;;          (map (fn [[ticket-rule nums]] [ticket-rule (map #(Integer/parseInt %) nums)]))
;;          (map (fn [[ticket-rule nums]] [ticket-rule (partition-all 2 nums)]))))

;;   ;; (defn)

;;   (defn ticket-valid?
;;     [ticket]
;;     (every? (fn [per-rule-ranges] (every? (fn [num] (every? (fn [[min max]] (<= min num max)) per-rule-ranges)) ticket)) (map second per-lineranges)))

;;   (ticket-valid? (first nearby-tickets))

;;   (def valid-tickets
;;     (->> nearby-tickets
;;          (filter ticket-valid?)))

;;   (->> (apply concat nearby-tickets)
;;        (remove (fn [num] (some (fn [[min max]] (<= min num max)) ranges)))
;;        (apply +)))
