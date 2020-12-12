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
     (apply *)))



  ;; pt 2


  (+ 1 1)

  (def nums (->> (map edn/read-string example-input)
                 sort
                 vec))
  (def nums (concat [0] nums [(+ 3 (last nums))]))
  (defn get-combos
    [charges so-far]
    ;; (println "start" charges so-far results)
    (let [most-recent-add (last so-far)
          reached-end? (= most-recent-add (last nums))]
      (cond
        reached-end?
        (conj results so-far)

        :else
        (let [something
              (apply concat
                     (let [last-charge (last so-far)]
                       (map-indexed
                        (fn [idx next]
                          ;; (loop [charges (drop (inc idx) charges)
                          ;;        so-far (conj so-far next)
                          ;;        results results])
                          ;; (println "start" charges so-far results idx next)
                          (let [something-else
                                (get-combos (drop (inc idx) charges) (conj so-far next) results)]
                            something-else))
                        (filter #(>= 3 (- % last-charge)) charges))))]

          something))))

  (defn get-combos-2
    [so-far charges results]
    (let [most-recent (last so-far)
          [num-1 & rest] charges
          [_ num-2 & rest-2] charges
          [_ _ num-3 & rest-3] charges]
      (filter some?
              [(when (>= 3 (- num-1 most-recent))
                 (recur (conj so-far num-1) rest results))
               (when (>= 3 (- num-1 most-recent)) (recur (conj so-far num-2) rest-2 results))
               (when (>= 3 (- num-1 most-recent)) (recur (conj so-far num-3) rest-3 results))])))

  (defn combos-3
    [prev nums cache]
    (loop [prev prev
           [num & rest] nums
           cache cache]
      (if (nil? num)
        cache
        (let [opts (get cache prev)
              combs (->> opts
                         (take-while #(>= 3 (- (first %) num)))
                         (map #(cons num %)))
              opts (take-while #(>= 3 (- (first %) num)) opts)]
          ;; (println "opts combs" opts combs)
          (recur num rest (assoc cache num (if (not (seq rest))
                                             combs
                                             (concat combs opts))))))))

  (def nums (sort > nums))

  (+ 1 1)
  (time (count (get (combos-3 (first nums) (rest nums) {(first nums) [[(first nums)]]}) 0)))

  (def memoized (memoize get-combos))

  ;; (memoized (rest nums) [(first nums)] [] {})
  (+ 1 1)

  (time (count (memoized (rest nums) [(first nums)] [])))

  (partition-all 8 1 [1 2 3 4 5 6 7 8]))

    ;; (if (not (seq rest))
    ;;   (conj results so-far)
    ;;   (reduce (fn [num next]
    ;;             (cond
    ;;               (not (seq rest))
    ;;               (conj results so-far)

    ;;               (<= 3 (- num prev))
    ;;               (recur num (drop (inc idx) rest) (conj so-far num) results)

    ;;               :else
    ;;               (recur num (drop (inc idx)))))))
