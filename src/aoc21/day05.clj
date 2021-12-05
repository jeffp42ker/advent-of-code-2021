(ns aoc21.day05
  (:require
   [aocd.core :as data]
   [clojure.string :as str]))

;; --- Day 5: Hydrothermal Venture ---
;; https://adventofcode.com/2021/day/5


(def sample "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn input->coord [inp]
  (->>
   (str/split inp #"\n")
   (mapv #(str/split % #" -> "))
   (mapv (fn [c]
           (mapv #(->> (str/split % #",")
                       (mapv (fn [s] (Long/parseLong s))))
                 c)))))

(comment
  
  (input->coord sample)
  
  )

(defn point-range [start end]
  (if (<= start end)
    (range start (inc end))
    (range start (dec end) -1)))

(defn straight-line-points [[[x1 y1] [x2 y2]]]
  (for [x (point-range x1 x2)
        y (point-range y1 y2)]
    [x y]))

(defn straight-line? [[[x1 y1] [x2 y2]]]
  (or (== x1 x2) (== y1 y2)))

(def input (data/input 2021 5))

;; Part 1

(defn part-1 [inp]
  (->> inp
       (filter straight-line?)
       (map straight-line-points)
       (apply concat)
       frequencies
       vals
       (filter #(> % 1))
       count))

(comment

  (part-1 (input->coord sample))
  (part-1 (input->coord input))

  )

;;Part 2

(defn diagonal-line? [[[x1 y1] [x2 y2]]]
  (or (== (- x1 y1) (- x2 y2)) (== (+ x1 y1) (+ x2 y2))))

(defn diagonal-line-points [[[x1 y1] [x2 y2]]]
  (map vector (point-range x1 x2) (point-range y1 y2)))

(defn line-points [[[x1 y1] [x2 y2]]]
  (if (straight-line? [[x1 y1] [x2 y2]])
    (straight-line-points [[x1 y1] [x2 y2]])
    (diagonal-line-points [[x1 y1] [x2 y2]])))

(comment

  (diagonal-line? [[1 1] [3 3]])
  (diagonal-line? [[7 9] [9 7]])

  (diagonal-line-points [[1 1] [3 3]])
  (diagonal-line-points [[7 9] [9 7]])

  )

(defn part-2 [inp]
  (->> inp
       (map line-points)
       (apply concat)
       frequencies
       vals
       (filter #(> % 1))
       count))

(comment

  (part-2 (input->coord sample))
  (part-2 (input->coord input))

  )
