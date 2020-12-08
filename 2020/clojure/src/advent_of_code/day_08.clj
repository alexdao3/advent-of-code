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
  "part 1"
  [instructions]
  (loop [{:keys [acc pos visited] :as return}
         {:acc 0
          :pos 0
          :visited #{}}]
    (if (visited pos)
      acc
      (let [next (nth instructions pos)]
        (recur (process-instruction return next))))))

(defn get-interchangeable
  [instructions]
  (reduce
   (fn [acc [action dir-value]]
     ;; (println acc action)
     (if (#{"jmp" "nop"} action)
       (-> acc
           (update action #(conj % (:index acc)))
           (update :index inc))
       (update acc :index inc)))
   {:index 0
    "jmp" []
    "nop" []}
   instructions))

(defn swap-instruction
  [[action dir-value]]
  (if (= "jmp" action)
    ["nop" dir-value]
    ["jmp" dir-value]))

(defn run-ops-2
  "part 2"
  [instructions swap-pos]
  (loop [{:keys [acc pos visited] :as return}
         {:acc 0
          :pos 0
          :visited #{}}]
    (cond (visited pos)
          nil

          (= pos (count instructions))
          {:acc acc
           :index pos}

          :else (let [next (nth instructions pos)
                      next (if (= swap-pos pos) (swap-instruction next) next)]
                  (recur (process-instruction return next))))))

(comment
  ;; pt 1
  (->> input
       (map #(str/split % #" "))
       (run-ops))

  ;; pt 2
  (let [interchangeables (->> input
                              (map #(str/split % #" "))
                              (get-interchangeable))]
    (println "SOLUTION?"
             (->> input
                  (map #(str/split % #" "))
                  (#(some (fn [idx] (run-ops-2 % idx)) (get interchangeables "jmp"))))))


  ;; pt 2


  (let [interchangeables (->> input
                              (map #(str/split % #" "))
                              (get-interchangeable))]
    (println "SOLUTION?"
             (->> input
                  (map #(str/split % #" "))
                  (#(some (fn [idx] (run-ops-2 % idx)) (get interchangeables "nop"))))))

  (->> input
       (map #(str/split % #" "))
       (#(some (fn [idx] (run-ops-2 % idx)) (get interchangeables "jmp"))))

  (first)
  (#(process-instruction {:acc 0
                          :pos 0
                          :visited #{}}
                         %)))
