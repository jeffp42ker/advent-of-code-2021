;; --- Day 3: Binary Diagnostic ---
;; https://adventofcode.com/2021/day/3

(ns aoc21.day03
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(def sample "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn input->vs [inp] (->> (str/split inp #"\n") (mapv #(str/split % #""))))

(defn transpose [inp]
  (apply mapv vector inp))

(def input (data/input 2021 3))

;; Part 1

(defn part-1 [inp]
  (let [gamma-rate-str (->> inp
                            transpose
                            (mapv frequencies)
                            (mapv (fn [m] (if (> (m "1") (m "0")) "1" "0")))
                            (str/join))
        epsilon-rate-str (->> gamma-rate-str
                             (mapv (fn [x] (if (= x \1) "0" "1")))
                              str/join)]
    (apply * (mapv #(Integer/parseInt % 2) [gamma-rate-str epsilon-rate-str]))))


(comment

(transpose (input->vs sample))  
(part-1 (input->vs sample))
(part-1 (input->vs input))

    )

;; Part 2

(defn bit-freq [inp n]
  (let [slice (nth (transpose inp) n)
        freq (->> (frequencies slice) (sort-by val #(compare %2 %1)))]
    {:slice-pos n
     :freq freq
     :most-common (if (= (val (first freq))
                         (val (second freq))) "1" (->> freq first key))
     :least-common (->> freq last key)
     :input inp}))

(defn rating [bit-criteria inp]
  (loop [slice-pos 0
         input (filter #(= (bit-criteria (bit-freq inp slice-pos))
                           (nth % slice-pos)) inp)]
    (if (= 1 (count input))
      (Integer/parseInt (->> input first str/join) 2)
      (recur (inc slice-pos)
             (filter #(= (bit-criteria (bit-freq input (inc slice-pos)))
                         (nth % (inc slice-pos))) input)))))

(defn part-2 [inp]
  (let [oxygen-generator-rating (rating :most-common inp)
        co2-scrubber-rating (rating :least-common inp)]
    (* oxygen-generator-rating co2-scrubber-rating)))


(comment

  (bit-freq (input->vs sample) 1)
  (rating :most-common (input->vs sample))
  (rating :least-common (input->vs sample))
  (part-2 (input->vs sample))
  (part-2 (input->vs input))


  )