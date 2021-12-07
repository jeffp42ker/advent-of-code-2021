(ns aoc21.day07
  (:require [aocd.core :as data]
            [clojure.string :as str]))

;; --- Day 7: The Treachery of Whales ---
;; https://adventofcode.com/2021/day/7

; massive underground cave system
; the horizontal position of each crab


(def sample "16,1,2,0,4,2,7,1,2,14")
(def input (data/input 2021 7))

(defn input->xs [inp] (mapv #(Long/parseLong %) (str/split (str/replace inp #"\n" "") #",")))

(defn abs [n] (max n (- n)))

; mean & median
; https://github.com/clojure-cookbook/clojure-cookbook/blob/master/01_primitive-data/1-20_simple-statistics.asciidoc

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway) ; (1)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn fuel [position starts]
  (apply + (map (fn [n] (abs (- position n))) starts)))

; Part 1

(defn part-1 [inp]
  (fuel (median inp) inp))
  
(comment

  (part-1 (input->xs sample))
  (part-1 (input->xs input))

  )

; Part 2

(defn fuel-cost [start end]
  (let [[start end] (if (> start end) [end start] [start end])]
    (loop [start start
           cost 0
           fuel 0]
      (if (== start end) fuel
          (recur (inc start) (inc cost) (+ fuel (inc cost)))))))

(defn fuel-2 [position starts]
  (apply + (map (fn [n] (fuel-cost position n)) starts)))

(defn candidate-positions [inp]
  (range (reduce min inp) (inc (reduce max inp))))

(defn part-2 [inp]
  (->>
   (map #(fuel-2 % inp) (candidate-positions inp))
   (reduce min)))


(comment

  (part-2 (input->xs sample))
  (part-2 (input->xs input))

  (candidate-positions (input->xs sample))
  (fuel-2 5 (input->xs sample))

  (fuel-cost 0 5)
  (fuel-cost 16 5)

  )


