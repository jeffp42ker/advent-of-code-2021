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

(defn slice [n inp]
  (mapv (fn [v] (nth v n)) inp))

(defn slice-freq [n inp]
  (let [freq (->> inp
                  (slice n)
                  frequencies
                  (sort-by val #(compare %2 %1)))
        tie? (apply = (vals freq))]
    {:most-common (if tie? "1" (-> freq first key))
     :least-common (-> freq last key)}))

(defn slice-rate [criteria n inp]
  (criteria (slice-freq n inp)))

(defn rate [criteria inp]
  (let [xs (range (count (first inp)))]
    (apply str (mapv #(slice-rate criteria % inp) xs))))

(comment
  
  (slice 0 (input->vs sample))
  (slice-freq 0 (input->vs sample))
  (rate :most-common (input->vs sample))

  )

(def input (data/input 2021 3))

;; Part 1

(defn part-1 [inp]
  (let [[gamma-rate epsilon-rate] (mapv #(rate % inp) [:most-common :least-common])
        power-consumption (apply * (mapv #(Integer/parseInt % 2)
                                         [gamma-rate epsilon-rate]))]
    power-consumption))


(comment

(part-1 (input->vs sample))
(part-1 (input->vs input))

    )

;; Part 2

(defn rating [bit-criteria inp]
  (loop [pos 0
         input (filter #(= (bit-criteria (slice-freq pos inp))
                           (nth % pos)) inp)]
    (if (= 1 (count input))
      (Integer/parseInt (->> input first str/join) 2)
      (recur (inc pos)
             (filter #(= (bit-criteria (slice-freq (inc pos) input))
                         (nth % (inc pos))) input)))))

(defn part-2 [inp]
  (let [oxygen-generator-rating (rating :most-common inp)
        co2-scrubber-rating (rating :least-common inp)
        life-support-rating
        (* oxygen-generator-rating co2-scrubber-rating)]
    life-support-rating))


(comment

  (slice-freq 1 (input->vs sample))

  (rating :most-common (input->vs sample))
  (rating :least-common (input->vs sample))

  (part-2 (input->vs sample))
  (time (part-2 (input->vs input)))

  )