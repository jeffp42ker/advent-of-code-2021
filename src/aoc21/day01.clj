(ns aoc21.day01
"--- Day 1: Sonar Sweep --- https://adventofcode.com/2021/day/1 "
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(def sample "199
200
208
210
200
207
240
269
260
263")

(def input (data/input 2021 1))

(defn input->xs [inp]
  (mapv #(Integer/parseInt %) (str/split inp #"\n")))

(defn part-1
  [inp]
  (let [xs (input->xs inp)]
    (->> (mapv - (next xs) xs)
         (filter pos?)
         count)))

(comment

  (part-1 sample) ; 7
  (part-1 input) ; 1374

  )

(defn part-2
  [inp]
  (let [xs (input->xs inp)
        windows (mapv + xs (next xs) (nnext xs))]
    (->> (mapv - (next windows) windows)
         (filter pos?)
         count)))

(comment

  (part-2 sample) ; 5
  (part-2 input) ; 1418
  
  )


;; minikomi
;; https://www.reddit.com/r/adventofcode/comments/r66vow/2021_day_1_solutions/hmrfnhv/

(defn advent-1 [vs]
  (->> vs
       (partition 2 1)
       (filter #(apply < %))
       (count)))

(defn advent-2 [vs]
  (->> vs
       (partition 3 1)
       (map #(apply + %))
       (advent-1)))


(comment

(advent-1 (input->xs sample)) ; 7
(advent-1 (input->xs input)) ; 1374 

(advent-2 (input->xs sample)) ; 5
(advent-2 (input->xs input)) ; 1418 

)