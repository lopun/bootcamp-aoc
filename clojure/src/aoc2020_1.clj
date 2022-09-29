(ns aoc2020_1
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [file-util :refer [read-file-as-list]]
            [clojure.math.combinatorics :as combo]))

(defn combination-2
  [elems]
  (->> (for [x elems y elems] (when-not (= x y) #{x y}))
       (keep identity)
       distinct))

(defn combination-3
  [elems]
  (->> (for [x elems y elems z elems]
         (when-not (or (= x y) (= x z) (= y z)) #{x y z}))
       (keep identity)
       distinct)) ;; 그냥 keep은 안되는 것 같습니다

(defn find-matching-sum-then-multiply
  [sum-value set-value]
  (when (= (reduce + set-value) sum-value) (reduce * set-value)))

(defn part1-solution
  []
  (->> (read-file-as-list "aoc2020_1.txt")
       (map #(Integer/parseInt %))
       combination-2
       (keep #(find-matching-sum-then-multiply 2020 %))
       first))

(defn part2-solution
  []
  (->> (read-file-as-list "aoc2020_1.txt")
       (map #(Integer/parseInt %))
       combination-3
       (keep #(find-matching-sum-then-multiply 2020 %))
       first))

(comment
  (combination-2 [1 2 3 4 5])
  (combination-3 [1 2 3 4 5])
  ((fn [[a b]] a) #{1 2})
  (reduce + #{1 2})
  (part1-solution)
  (part2-solution)
  (combo/combinations [1 2 3] 2))