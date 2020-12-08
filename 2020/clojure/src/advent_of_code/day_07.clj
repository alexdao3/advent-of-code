(ns advent-of-code.day-07
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn part-1
  "Day 07 Part 1"
  [input]
  input)

(defn part-2
  "Day 07 Part 2"
  [input]
  input)

(def input
  (-> (slurp "resources/day-07.txt")
      (str/split #"\n")))

;; gross. map of bag -> contents of bag
(def bag->contents
  (apply
   merge
   (map
    (fn [line]
      (let [tokens (->> (str/split line #" contain ")
                        (map #(-> (str/replace % #"[.]" "")
                                  (str/split #", ")))
                        (apply concat))
            [container & contained] tokens
            container (keyword (str/join  "-" (drop-last (str/split container #" "))))
                   ;; container (keyword (str/replace container #" " "-"))
            contained (map #(str/split % #" ") contained)
            contained (reduce
                       (fn [result [amt & description]]
                         (assoc result
                                (keyword (str/join "-" (drop-last description)))
                                (if (= amt "no")
                                  nil
                                  (Integer/parseInt amt))))
                       {}
                       contained)]
        {container contained}))
    input)))

(defn find-color
  "pt 1"
  [bag target]
  (loop [found? false
         bags  #{bag}]
         ;; bag-contents (bag->contents bag)
         ;; paths [#{bag}


    ;; if bags contains thing, return true
    (cond
      found? true

      (not (seq bags)) false

      :else
      (recur
       (bags target)
       (set
        (apply
         concat
         (for [bag bags]
           (keys (bag->contents bag)))))))))

(defn find-num-bags
  "pt 2"
  [bag]
  (loop [num-bags 0
         bags  #{bag}]
    (cond
      (not (seq bags)) num-bags

      :else
      (let [contents
            (apply concat
                   (for [bag bags]
                     (bag->contents bag)))
            next-bags (apply concat
                             (for [[bag num-bags] (filter (comp second) contents)]
                               (repeat num-bags bag)))
            num (reduce + (keep second contents))]
        (recur
         (+ num num-bags)
         next-bags)))))

(comment
  ;; pt1
  (->> (disj (set (keys bag->contents)) :shiny-gold)
       (filter #(find-color % :shiny-gold))
       count)

  ;; pt2
  (find-num-bags :shiny-gold))
