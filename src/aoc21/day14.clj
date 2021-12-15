(ns aoc21.day14
  (:require [aocd.core :as data]
            [clojure.string :as str]))

;; --- Day 14: Extended Polymerization ---
;; https://adventofcode.com/2021/day/14

; polmer template
; pair insertion rule list

(def sample "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn input->template-n-rules [inp]
  (let [[template rules] (str/split inp #"\n\n")
        rules (->> rules str/split-lines 
                   (map #(re-find #"(\w+) -> (\w)" %))
                   (map (fn [[_ [a b] [c]]] [[a b] c]))
                   (into {}))]
    {:template template :rules rules}))

;; studying zelark's solution for performance
;; https://github.com/zelark/AoC-2021/blob/main/src/zelark/aoc_2021/day_14.clj

(defn apply-rules [freq rules]
  (reduce-kv (fn [m [a b :as p] v]
               (if-let [c (get rules p)]
                 (-> m
                     (update [a c] (fnil + 0) v)
                     (update [c b] (fnil + 0) v))
                 m))
             {}
             freq))
   
(def input (data/input 2021 14))

;; Part 1

(defn part-1 [inp n]
  (let [{:keys [template rules]} inp
        pairs (partition 2 1 (:template inp))]
    (->> (frequencies pairs)
         (iterate #(apply-rules % rules))
         (drop n)
         first
         (reduce-kv (fn [m [a] v] (update m a (fnil + 0) v))
                    {(last template) 1})
         vals
         (apply (juxt max min))
         (apply -))))  

(comment

  (part-1 (input->template-n-rules sample) 10)
  (part-1 (input->template-n-rules input) 10)

)

;; Part 2

(comment

  (part-1 (input->template-n-rules sample) 40)
  (part-1 (input->template-n-rules input) 40))


      



    