(ns advent-of-code.day-12
  (:require [clojure.string :as str]))

(defn part-1
  "Day 12 Part 1"
  [input]
  input)

(defn part-2
  "Day 12 Part 2"
  [input]
  input)

(def input
  (str/split (slurp "resources/day-12.txt") #"\n"))

(defn deg->direction
  [deg]
  (let [rem (mod deg 360)]
    (case rem
      0   "E"
      90  "S"
      180 "W"
      270 "N")))

(defn line->instruction
  [line]
  (let [cmd (subs line 0 1)
        num (Integer/parseInt (subs line 1))]
    (case cmd
      "E" {:dx num}
      "W" {:dx (- 0 num)}
      "N" {:dy num}
      "S" {:dy (- 0 num)}
      "L" {:ddeg (- 0 num)}
      "R" {:ddeg num}
      "F" {:fwd num})))

;; pt 1

(defn fwd->next-location
  [{deg :deg :as current} {fwd :fwd :as fwd-instruction}]
  (let [direction (deg->direction deg)
        fwd (if (#{"W" "S"} direction)
              (- 0 fwd)
              fwd)]

    (cond
      (#{"E" "W"} direction)
      (update current :x + fwd)

      :else
      (update current :y + fwd))))

(defn instruction->next-location
  [{:keys [deg] :as current} {:keys [dx dy ddeg fwd] :as instruction}]
  (println current instruction)
  (cond-> current

    dx
    (update :x + dx)

    dy
    (update :y + dy)

    ddeg
    (update :deg + ddeg)

    fwd
    (fwd->next-location instruction)))

(defn instructions->destination
  [start instructions]
  (reduce
   instruction->next-location
   start
   instructions))

;; pt 2
(defn deg->updated-wp
  [deg {:keys [wp-x wp-y]}]
  (let [rem (mod deg 360)]
    (case rem
      0   {:wp-x wp-x
           :wp-y wp-y}
      90  {:wp-x wp-y
           :wp-y (- wp-x)}
      180 {:wp-x (- wp-x)
           :wp-y (- wp-y)}
      270 {:wp-x (- wp-y)
           :wp-y wp-x})))

(defn fwd-wp->next-location
  [{deg :deg wp-x :wp-x wp-y :wp-y :as current} {fwd :fwd :as fwd-instruction}]
  (let [dx (* fwd wp-x)
        dy (* fwd wp-y)]
    (-> current
        (update :x + dx)
        (update :y + dy))))

(defn instruction-wp->next-location
  [{:keys [deg] :as current} {:keys [dx dy ddeg fwd] :as instruction}]
  (println current instruction)
  (cond-> current

    dx
    (update :wp-x + dx)

    dy
    (update :wp-y + dy)

    ddeg
    ((fn [current]
       (merge current (deg->updated-wp ddeg current))))

    fwd
    (fwd-wp->next-location instruction)))

(defn instructions-wp->destination
  [start instructions]
  (reduce
   instruction-wp->next-location
   start
   instructions))

(defn get-abs
  [n]
  (max n (- n)))

(defn get-manhattan-distance
  [{:keys [x y]}]
  (+ (get-abs x) (get-abs y)))

(comment
  ;; pt1
  (->> (map line->instruction input)
       (instructions->destination {:x 0 :y 0 :deg 0})
       (get-manhattan-distance))

  ;; pt2
  (->> (map line->instruction input)
       (instructions-wp->destination {:x 0 :y 0 :deg 0 :wp-x 10 :wp-y 1})
       (get-manhattan-distance)))

input

(deg->direction 0)
