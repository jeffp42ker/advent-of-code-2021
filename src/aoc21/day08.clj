(ns aoc21.day08
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 8: Seven Segment Search ---
;; https://adventofcode.com/2021/day/8

; https://en.wikipedia.org/wiki/Seven-segment_display
; segments
; all ten unique signal patterns
; four digit output value
; Counting only digits in the output values

(def sample "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def input (data/input 2021 8))

(defn input->output-signals [inp]
  (->> (str/split inp #"\n")
       (map #(str/split % #" \| "))
       (map second)
       (mapv #(str/split % #"\s"))))

(defn segment->unique-digit [s]
  (let [encoding {2 1 4 4 3 7 7 8}]
    (encoding (count s))))

; Part 1

(defn part-1 [inp]
  (->> inp
       (apply concat)
       (map segment->unique-digit)
       (remove nil?)
       count))

(comment

  (part-1 (input->output-signals sample))
  (part-1 (input->output-signals input))
  
  )

; Part 2

(def small-sample "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(def small-sample-patterns "acedgfb: 8
cdfbe: 5
gcdfa: 2
fbcad: 3
dab: 7
cefabd: 9
cdfgeb: 6
eafb: 4
cagedb: 0
ab: 1")

(defn input->patterns_outputs [inp]
  (->> (str/split inp #"\n")
       (mapv #(str/split % #"[\s\|]+"))
       (mapv (fn [xs] [(vec (take 10 xs)) (vec (take-last 4 xs))]))))

(defn output-value->unique-digit-patterns [xs]
  (->> (map (fn [s] [(set s) (segment->unique-digit s)]) xs)
       (remove #(nil? (second %)))
       (into {})))

(defn output-value [inp]
  (let [[patterns outputs] inp
        m-uniq (set/map-invert (output-value->unique-digit-patterns patterns))
        mapping (set/map-invert
                 (reduce
                  (fn [m s]
                    (cond (and (= 6 (count s))
                               (= (m 8) (set/union (m 7) (set s)))) (assoc m 6 (set s))
                          (and (= 6 (count s))
                               (= (set s) (set/union (m 4) (set s)))) (assoc m 9 (set s))
                          (and (= 6 (count s))
                               (= (m 8) (set/union (m 4) (set s)))) (assoc m 0 (set s))
                          (and (= 5 (count s))
                               (= (set s) (set/union (m 7) (set s)))) (assoc m 3 (set s))
                          (and (= 5 (count s))
                               (= (m 8) (set/union (m 4) (set s)))) (assoc m 2 (set s))
                          (and (= 5 (count s))
                               (and (not= (m 8) (set/union (m 4) (set s)))
                                    (not= (set s) (set/union (m 7) (set s))))) (assoc m 5 (set s))
                          :else m))
                  m-uniq
                  patterns))]
         (mapv #(mapping (set %)) outputs)#_[outputs mapping]))

(defn part-2 [inp]
  (->> inp
       (map output-value)
       (map #(Long/parseLong (apply str %)))
       (reduce +)))

(comment

  (part-2 (input->patterns_outputs sample))
  (part-2 (input->patterns_outputs input))

)

