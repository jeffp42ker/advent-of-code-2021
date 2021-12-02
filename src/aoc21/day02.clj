(ns aoc21.day02
  "--- Day 2: Dive! --- https://adventofcode.com/2021/day/2 "
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(def sample "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(def input (data/input 2021 2))

(defn input->xs [inp]
  (mapv (comp (fn [[_ dir n]] [dir (Long/parseLong n)])
              #(first (re-seq #"(\w+) (\d+)" %))) (str/split inp #"\n")))

(defn part-1 [inp]
  (let [fs {"forward" (fn [[x y] n] [(+ x n) y])
            "down" (fn [[x y] n] [x (+ y n)])
            "up" (fn [[x y] n] [x (- y n)])}
        rf (fn [coll [s n]] ((fs s) coll n))]
    (->> (reduce rf [0 0] (input->xs inp))
        (apply *))))

(comment

  (input->xs sample)
  (part-1 input)
  
  )

(defn part-2 [inp]
  (let [fs {"forward" (fn [[x y z] n] [(+ x n) (+ y (* n z)) z])
            "down" (fn [[x y z] n] [x y (+ z n)])
            "up" (fn [[x y z] n] [x y (- z n)])}
        rf (fn [coll [s n]] ((fs s) coll n))]
    (->> (reduce rf [0 0 0] (input->xs inp))
         (take 2)
         (apply *))))

(comment

  (part-2 sample)
  (part-2 input)

  )