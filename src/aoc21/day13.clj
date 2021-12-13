(ns aoc21.day13
  (:require [aocd.core :as data]
            [clojure.string :as str]))

;; --- Day 13: Transparent Origami ---
;; https://adventofcode.com/2021/day/12

(def sample "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn input->dots-n-folds [inp]
  (let [[dots folds] (str/split inp #"\n\n")
        dots (->> dots str/split-lines
                  (map #(str/split % #","))
                  (mapv (fn [[x y]] (mapv #(Long/parseLong %) [x y]))))
        folds (->> folds str/split-lines
                   (mapv #(re-find #"(\w)=(\d+)" %))
                   (mapv (fn [[_ c n]] [c (Long/parseLong n)])))]
    {:dots dots :folds folds}))

(def input (data/input 2021 13))

(defn fold-paper [{:keys [dots folds]}]
  (let [[axis line] (first folds)
        paper-size (* 2 line)
        f-move-up (fn [[x y]] [x (if (< line y) (- paper-size y) y)])
        f-move-left (fn [[x y]] [(if (< line x) (- paper-size x) x) y])
        f (if (= axis "x") f-move-left f-move-up)
        folded-dots (into #{} (mapv f dots))]
    (println [(count dots) (count folded-dots)])
    {:dots folded-dots
     :folds (vec (rest folds))
     :count-change [(count dots) (count folded-dots)]}))

;; Part 1

(defn part-1 [inp]
  (->> inp
   (input->dots-n-folds)
   (fold-paper)
   :count-change))

(comment
  
  (part-1 sample)
  (part-1 input)

  )

;; Part 2

(defn display-code [inp]
  (let [[rows cols]
        (reduce
         (fn [acc [x y]] [(max x (first acc)) (max y (second acc))])
         [0 0]
         inp)
        grid (vec (repeat (inc cols) (vec (repeat (inc rows) "."))))
        f (fn [acc [x y]]
            (assoc acc y (assoc (nth acc y) x "#")))]
    (reduce f grid inp)))

(defn part-2 [inp]
  (let [start (input->dots-n-folds inp)
        fold-count (count (:folds start))]
    (println fold-count)
    (->> start
         (iterate fold-paper)
         (take (inc fold-count))
         last
         :dots
         display-code
         (map #(apply str %))
         (str/join "\n")
         (spit "resources/day13/camera-activation-code.txt" ))))

(comment

  (part-2 sample)
  (part-2 input)

)
  