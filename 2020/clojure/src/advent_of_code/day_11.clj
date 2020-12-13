(ns advent-of-code.day-11
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "resources/day-11.txt")
      (str/split #"\n")))

(defn part-1
  "Day 11 Part 1"
  [input]
  input)

(defn part-2
  "Day 11 Part 2"
  [input]
  input)

(def seat-configuration
  (mapv #(str/split % #"") input))

(def num-cols (count (first seat-configuration)))
(def num-rows (count seat-configuration))

(defn get-seat
  [configuration x y]
  (get-in configuration [y x]))

(defn neighbors
  [x y]
  (if (or (>= y num-rows) (>= x num-cols)
          (< x 0)         (< y 0))
    []
    (for [x2 (range (max 0 (dec x)) (min (+ 2 x) num-cols))
          y2 (range (max 0 (dec y)) (min (+ 2 y) num-rows))
          :when (not (and (= y2 y) (= x2 x)))]
      [x2 y2])))

(defn expanding-neighbors
  [x y]
  (if (or (>= y num-rows) (>= x num-cols)
          (< x 0)         (< y 0))
    []
    {:tl (for [dx (range)] (let [steps (+ dx 1)] [(- x steps) (+ y steps)]))
     :t  (for [dx (range)] (let [steps (+ dx 1)] [x (+ y steps)]))
     :tr (for [dx (range)] (let [steps (+ dx 1)] [(+ x steps) (+ y steps)]))
     :r  (for [dx (range)] (let [steps (+ dx 1)] [(+ x steps) y]))
     :br (for [dx (range)] (let [steps (+ dx 1)] [(+ x steps) (- y steps)]))
     :b  (for [dx (range)] (let [steps (+ dx 1)] [x (- y steps)]))
     :bl (for [dx (range)] (let [steps (+ dx 1)] [(- x steps) (- y steps)]))
     :l  (for [dx (range)] (let [steps (+ dx 1)] [(- x steps) y]))}))

(defn get-first-seat
  [seat-configuration possible-neighbors]
  (some (fn [[x y]] (#{"L" "#"} (get-seat seat-configuration x y)))
        (take-while (fn [[x y]]
                      (and (<= 0 x num-cols)
                           (<= 0 y num-rows)))
                    possible-neighbors)))

(defn get-expanding-neighbors->occupied-neighbors
  [configuration x y]
  (reduce (fn [num-occupied [direction possible-seats]]
            (let [first-seat (get-first-seat configuration possible-seats)]
              (+ num-occupied
                 (if (= "#" first-seat)
                   1
                   0))))
          0
          (expanding-neighbors x y)))

;; (some (fn [[x y]] (#{"L" "#"} (get-seat seat-configuration x y)))
;;       (take-while (fn [[x y]]
;;                     (and (<= 0 x num-cols)
;;                          (<= 0 y num-rows)))
;;                   (:tl (expanding-neighbors 1 1))))

(get-expanding-neighbors->occupied-neighbors seat-configuration 0 0)
;; If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
;; If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
;; Otherwise, the seat's state does not change.
(defn seat->occupied-neighbors
  [configuration x y]
  (reduce (fn [acc [next-x next-y]]
            (let [neighbor (get-seat configuration next-x next-y)]
              (+ acc
                 (if (= neighbor "#")
                   1
                   0))))
          0
          (neighbors x y)))

(defn seat->next-seat
  [configuration x y]
  (let [seat (get-seat configuration x y)
        ;; pt 1
        occupied-neighbors (seat->occupied-neighbors configuration x y)
        ;; pt2
        occupied-neighbors (get-expanding-neighbors->occupied-neighbors configuration x y)]
    (cond
      (and (= seat "L") (zero? occupied-neighbors))
      "#"

      (and (= seat "#") (<= 5 occupied-neighbors))
      "L"

      :else
      seat)))

(seat->next-seat seat-configuration 0 0)
(seat->next-seat seat-configuration 0 0)
(seat->next-seat seat-configuration 0 0)

(defn next-configuration
  [start-configuration]
  (reduce
   (fn [configuration [x y]]
     (assoc-in configuration [y x] (seat->next-seat start-configuration x y)))
   start-configuration
   (for [x (range num-cols)
         y (range num-rows)]
     [x y])))

(defn generate-configuration
  [start]
  (loop [configuration start]
    (let [next (next-configuration configuration)]
      (if (= configuration next)
        configuration
        (recur next)))))

(next-configuration (next-configuration (next-configuration seat-configuration)))
(time
 (->> (generate-configuration seat-configuration)
      (reduce (fn [occupied next-row]
                (->> next-row
                     (filter (fn [seat] (= "#" seat)))
                     count
                     (+ occupied)))
              0)))
