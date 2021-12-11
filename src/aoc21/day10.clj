(ns aoc21.day10
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.set :as set]))

(def sample "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

;; Part 1

(defn input->vs [inp] (str/split inp #"\n"))

(def input (data/input 2021 10))

(def open->close {\( \) \[ \] \{ \} \< \>})
(def close->open (set/map-invert open->close))

(defn kissing? [l r]
    (= l (close->open r)))

(defn opening? [c]
  (.contains (keys open->close) c))

(defn closing? [c]
  (.contains (keys close->open) c))

(defn remove-balanced [vs]
   (reduce (fn [acc c]
             (if (and (closing? c) (kissing? (peek acc) c))
               (vec (pop acc))
               (vec (conj acc c))))
           []
           vs))

(defn corrupt? [s]
  (->> (into [] s)
             remove-balanced
             (filter closing?)
       first))

(defn score [c]
  ({\) 3
    \] 57
    \} 1197
    \> 25137} c))

(defn part-1 [inp]
  (->> inp
       (map corrupt?)
       (map score)
       (remove nil?)
       (apply +)))


(comment
  
  (part-1 (input->vs sample))
  (part-1 (input->vs input))

  )

;; Part 2

(defn score-2 [s]
  (let [points {\) 1 \] 2 \} 3 \> 4}]
    (->> s
         (into [])
         (remove-balanced)
         (reduce str)
         (reverse)
         (mapv #((merge open->close close->open) %))
         (reduce (fn [x c] (+ (* x 5) (points c)))
                 0))))


(defn part-2 [inp]
  (let [scores (->> inp
                    (remove corrupt?)
                    (map #(score-2 %))
                    sort)]
    (nth scores (/ (count scores) 2))))

(comment

  (part-2 (input->vs sample))
  (part-2 (input->vs input))
  
  )