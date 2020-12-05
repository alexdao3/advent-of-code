(ns advent-of-code.day-05
  (:require [clojure.string :as str]))

(defn binary-str->decimal
  [binary-str {:keys [max-v lower-key upper-key]}]
  (loop [{:keys [min-v max-v]} {:min-v 0 :max-v max-v}
         [next & left] binary-str]
    (if (nil? next)
      (min min-v max-v)
      (recur
       (cond-> {:min-v min-v
                :max-v max-v}
         (= lower-key next)
         (assoc :max-v (Math/floor (- max-v (/ (- max-v min-v) 2))))

         (= upper-key next)
         (assoc :min-v (Math/ceil (- max-v (/ (- max-v min-v) 2)))))
       left))))

(defn seat-str->seat
  [seat-str]
  (let [row (binary-str->decimal (subs seat-str 0 7) {:max-v 127 :lower-key \F :upper-key \B})
        col (binary-str->decimal (subs seat-str 7) {:max-v 7 :lower-key \L :upper-key \R})]
    (+ col (* row 8))))

(defn part-1
  "Day 05 Part 1"
  [input]
  (let [lines (-> (slurp input) 
                  (str/split #"\n"))]
    (->> lines
         (map seat-str->seat)
         (sort-by identity)
         (apply max))))

(defn part-2
  "Day 05 Part 2"
  [input]
  (let [lines (-> (slurp input)
                  (str/split #"\n"))]
    (->> lines
     (map seat-str->seat)
     (sort-by identity)
     (partition 2)
     (some (fn [[prev next]]
             (when (not= (inc prev) next)
               (inc prev)))))))


(comment
  ;;  part 1
  (part-1 "resources/day-05.txt")
  (part-2 "resources/day-05.txt")
  (-> (slurp "resources/day-05.txt")
      (str/split #"\n")
      ;; first
      (#(map seat-str->seat %))
      (#(sort-by identity %))
      (#(apply max %)))

  ;;  part 2
  (-> (slurp "resources/day-05.txt")
      (str/split #"\n")
      ;; first
      (#(map seat-str->seat %))
      (#(sort-by identity %))
      (#(partition 2 %))
      (#(some (fn [[prev next]]
                (when (not= (inc prev) next)
                  (inc prev)))
              %)))
                  

  (seat-str->seat "FBFBBFFRLR")

  (loop [{:keys [min-v max-v]} {:min-v 0 :max-v 127}
         [next & left] "FBFBBFF"]
    (println min-v max-v next left)
    (if (nil? next)
      (min min-v max-v)
      (recur
       (cond-> {:min-v min-v
                :max-v max-v}
         (= \F next)
         (assoc :max-v (Math/floor (- max-v (/ (- max-v min-v) 2))))

         (= \B next)
         (assoc :min-v (Math/ceil (- max-v (/ (- max-v min-v) 2)))))
       left))))
