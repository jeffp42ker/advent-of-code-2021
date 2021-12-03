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

(defn input->vs [inp]
  (mapv (comp (fn [[_ dir n]] [(keyword dir) (Long/parseLong n)])
              #(re-matches #"(\w+) (\d+)" %))
        (str/split inp #"\n")))

(defn part-1 [inp]
  (let [rf (fn [[x y] [dir n]]
             (case dir
               :forward [(+ x n) y]
               :down [x (+ y n)]
               :up [x (- y n)]))]
    (->> (reduce rf [0 0] (input->vs inp))
         (apply *))))

(comment

  (input->vs sample)
  (part-1 sample)
  (part-1 input)
  
  )


(defn part-2 [inp]
  (let [rf (fn [[x y aim] [dir n]]
             (case dir
               :forward [(+ x n) (+ y (* n aim)) aim]
               :down [x y (+ aim n)]
               :up [x y (- aim n)]))]
    (->> (reduce rf [0 0 0] (input->vs inp))
         (take 2)
         (apply *))))

(comment

  (part-2 sample)
  (part-2 input)

  )