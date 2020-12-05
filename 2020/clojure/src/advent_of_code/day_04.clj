(ns advent-of-code.day-04
  (:require [clojure.string :as str]))

(def required
  #{"ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"})

(def num-fields
  #{"byr" "iyr" "eyr"})

(defn validate-field
  [[attr value]]
  (let [value (if (num-fields attr) (Integer/parseInt value) value)]
    (case attr
      "byr" (<= 1920 value 2002)
      "iyr" (<= 2010 value 2020)
      "eyr" (<= 2020 value 2030)
      "hgt" (let [unit (subs value (- (count value) 2))
                  num (subs value 0 (- (count value) 2))
                  num (if (seq num)
                        (Integer/parseInt num)
                        0)]
              (and
               (seq unit)
               (if (= unit "cm")
                 (<= 150 num 193)
                 (<= 59 num 76))))
      "hcl" (and (= 7 (count value))
                 (seq (re-matches #"#[\da-fA-F]{6}" value)))
      "ecl" (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} value)
      "pid" (and
             (= 9 (count value))
             (seq (re-matches #"\d+" value)))
      "cid" true)))

(defn get-valid-passports
  [passports validate-passport-fn]
  (->> passports
       (map (fn [passport]
              (let [kv-pairs (str/split passport #"\s+")]
                (->> kv-pairs
                     (keep #(let [pair (str/split % #":")] 
                              (when (validate-passport-fn pair)
                                (first pair))))
                     set))))
       (filter (fn [attrs] (every? attrs required)))
       count))
  

(defn part-1
  "Day 04 Part 1"
  [input]
  (let [entries (-> (slurp input)
                    (str/split #"\n\n"))]
    (->> (get-valid-passports entries identity))))
                              
                            
(defn part-2
  "Day 04 Part 2"
  [input]
  (let [entries (-> (slurp input)
                    (str/split #"\n\n"))]
    (get-valid-passports entries validate-field)))
                            

(comment
  (part-1 "resources/day-04.txt")
  (part-2 "resources/day-04.txt")
  (<= 2010 (Integer/parseInt "2020") 2030)
  (re-matches #"#[\da-fA-F]{6}" "#AB1DEF")
  (re-matches #"\d+" "000000000")
  (subs "123in" (- (count "123in") 2))
  (-> (slurp "resources/day-04.txt")
      (str/split #"\n\n")
      ;; (#(take 5 %))
      (#(map (fn [entry] (str/split entry #"\s+")) %)) 
      (#(map (fn [entry] (set (map (fn [inner-entry] (let [kv-pair (str/split inner-entry #":")]
                                                       (when (validate-field kv-pair)
                                                         (first kv-pair))))
                                   entry))) %))
      (#(filter (fn [attrs] (every? attrs required)) %))
      count))
      ;; first
      
