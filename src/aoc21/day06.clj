(ns aoc21.day06
  (:require
   [aocd.core :as data]
   [clojure.string :as str]))

;; --- Day 6: Lanternfish ---
;; https://adventofcode.com/2021/day/6

; gestation period 7 days; :new +2 days
; the number of days until it creates a new lanternfish.
; maybe exponentially quickly?

(def sample "3,4,3,1,2")

(defn input->s [inp] (apply str (filter #(Character/isDigit %) inp)))
(defn input->xs [inp] (map #(Long/parseLong %) (str/split (str/replace inp "\n" "") #",")))

(def input (data/input 2021 6))


;; Part 1

(defn part-1 [[s countdown]]
  (loop [s s
         countdown countdown]
    (if (= countdown 0)
      (count s)
      (let [spawns ((frequencies s) \0)
            new (and spawns (apply str (repeat ((frequencies s) \0) 8)))
            next-day (->> s
                          (map #(Long/parseLong (str %)))
                          (map #(if (= 0 %) 6 (dec %)))
                          (apply str))]
        (recur (apply str next-day new) (dec countdown))))))

(comment
  
  (time (part-1 [(input->s sample) 80]))
  (time (part-1 [(input->s input) 80]))

  )

;; Part 2

(defn progress [fish]
  (reduce-kv
   (fn [fish' day count]
     (if (zero? day)
       (-> fish'
           (assoc 8 count)
           (update 6 (fnil + 0) count))
           (-> fish'
           (update (dec day) (fnil + 0) count))))
   {}
   fish))

(defn part-2 [inp days]
  (loop [fish (frequencies inp)
         day 0]
    (if (>= day days)
      (reduce + (vals fish))
      (recur (progress fish) (inc day)))))


(comment

(part-2 (input->xs sample) 256)
(part-2 (input->xs input) 256)

)



