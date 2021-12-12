(ns aoc21.day12
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.set :as set]
            [weavejester.dependency :as dep]))


;; --- Day 12: Passage Pathing ---
;; https://adventofcode.com/2021/day/12


(def sample "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def larger-sample "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def even-larger-sample "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(def input (data/input 2021 12))

(defn input->pairs [inp]
  (->> (str/split inp #"\n")
       (map #(str/split % #"-"))
       #_(mapv (fn [[a b]] (mapv keyword [a b])))))

(defn input->graph [inp]
  (->> inp (input->pairs)
       (reduce (fn [m [a b]]
                 (-> m
                     (update a (fnil conj #{}) b)
                     (update b (fnil conj #{}) a)))
               {})))

(defn input->dependencies [inp]
  (->> inp (input->pairs)
       (mapv (fn [[a b]]
               (hash-map :task b :depends-on a)))))


;; https://dnaeon.github.io/graphs-and-clojure/

(defn visited?
  "Predicate which returns true if the node v has been visited already, false otherwise."
  [v coll]
  (some #(= % v) coll))

(defn find-neighbors
  "Returns the sequence of neighbors for the given node"
  [v coll]
  (get coll v))

(defn graph-dfs
  "Traveres a graph in Depth First Search (DFS)"
  [graph v]
  (loop [stack (vector v) ;; Use a stack to store nodes we need to explore
         visited []]       ;; A vector to store the sequence of visisted nodes
    (if (empty? stack)      ;; Base case - return visited nodes if the stack is empty
        visited
        (let [v (peek stack)
              neighbors (find-neighbors v graph)
              not-visited (filter (complement #(visited? % visited)) neighbors)
              new-stack (into (pop stack) not-visited)]
          (if (visited? v visited)
            (recur new-stack visited)
            (recur new-stack (conj visited v)))))))

(defn graph-bfs
  "Traverses a graph in Breadth First Search (BFS)"
  [graph v]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY v)
         visited []]
    (if (empty? queue) visited
        (let [v (peek queue)
              neighbors (find-neighbors v graph)
              not-visited (filter (complement #(visited? % visited)) neighbors)
              new-queue (apply conj (pop queue) not-visited)]
          (if (visited? v visited)
            (recur new-queue visited)
            (recur new-queue (conj visited v)))))))

(defn make-dependency-graph
  "Makes a dependency graph from the given collection of tasks"
  [coll]
  (reduce (fn [g x] (dep/depend g (:task x) (:depends-on x)))
          (dep/graph)
          coll))

(def g (make-dependency-graph (input->dependency-graph sample)))

(defn lexical-comparator
  "A comparator which compares x and y in lexical order"
  [x y]
  (compare (name x) (name y)))

(comment

  (make-dependency-graph (input->dependencies sample))

  (->> sample (input->dependencies)
       (make-dependency-graph)
       (dep/topo-sort lexical-comparator))

  (graph-dfs (input->graph sample) "start")
  (graph-bfs (input->graph sample) "start")
  (graph-dfs (input->graph larger-sample) "start")
  (graph-bfs (input->graph larger-sample) "start")
  (graph-dfs (input->graph even-larger-sample) "start")
  (graph-bfs (input->graph even-larger-sample) "start")

  )

;; Part 1

; studied jarohen solution:
; https://github.com/jarohen/advent-of-code/blob/master/2021/src/aoc2021/day12.clj

(defn paths [init-q ->more-q]
  (loop [[{:keys [path cur] :as q} & more-q] init-q
         paths #{}]
    (cond
      (nil? q) paths
      (= "end" cur) (recur more-q (conj paths path))
      :else (recur (into more-q (->more-q q)) paths))))

(def conns (input->graph sample))

(defn test-path [path small-caves cur]
  (for [to (get conns cur)
        :let [small-cave? (Character/isLowerCase (first to))]
        :when (or (not small-cave?)
                  (not (contains? small-caves to)))]
    {:path (conj path to)
     :small-caves (cond-> small-caves small-cave? (conj to))
     :cur to}))

(comment

  (get conns "c")
  (test-path ["start"] #{"start"} "start")
  
  )


(defn paths [init-q ->more-q]
  (loop [[{:keys [path cur] :as q} & more-q] init-q
         paths #{}]
    (cond
      (nil? q) paths
      (= "end" cur) (do #_(println [more-q (conj paths path)])
                        (recur more-q (conj paths path)))
      :else 
      (do #_(println [(into more-q (->more-q q)) paths])
          (recur (into more-q (->more-q q)) paths)))))

(defn p1 [conns]
  (-> (paths [{:path ["start"], :small-caves #{"start"}, :cur "start"}]
             (fn [{:keys [path small-caves cur]}]
               (for [to (get conns cur)
                     :let [small-cave? (Character/isLowerCase (first to))]
                     :when (or (not small-cave?)
                               (not (contains? small-caves to)))]
                 {:path (conj path to)
                  :small-caves (cond-> small-caves small-cave? (conj to))
                  :cur to})))
      count))


(comment

  (p1 (input->graph sample))
  (p1 (input->graph input))

_)

 ;; Part 2
 
(defn p2 [conns]
  (-> (paths [{:path ["start"]
               :small-caves #{"start"}
               :double-yet? false
               :cur "start"}]
             (fn [{:keys [path small-caves double-yet? cur]}]
               (for [to (get conns cur)
                     :let [small-cave? (Character/isLowerCase (first to))]
                     :when (or (not small-cave?)
                               (not (contains? small-caves to))
                               (and (not double-yet?) (not= "start" to)))]
                 {:path (conj path to)
                  :small-caves (cond-> small-caves small-cave? (conj to))
                  :double-yet? (or double-yet? (contains? small-caves to))
                  :cur to})))
      count)) 

(comment


  (p2 (input->graph sample))
  (p2 (input->graph larger-sample))
  (p2 (input->graph even-larger-sample))
  (p2 (input->graph input))  
  
  )