(ns aoc2018_7
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [file-util :refer [read-file-as-list]]
            [clojure.algo.generic.functor :refer [fmap]]))

(defn parse-step
  [step-text]
  (->> step-text
       (re-find #"Step (\w) must be finished before step (\w) can begin.")
       rest
       ((fn [[from to]] {:from from :to to}))))

(defn group-steps
  [steps]
  (->> steps
       (group-by :from)
       (fmap #(map :to %))))

(defn get-start-letter
  [step-group]
  (let [all-letters (into #{} (concat (keys step-group) (flatten (vals step-group))))
        val-letters (into #{} (flatten (vals step-group)))]
    (first (set/difference all-letters val-letters))))

(defn part1-solution
  []
  (->> (read-file-as-list "aoc2018_7.txt")
       (map parse-step)
       group-steps
       get-start-letter))


(comment
  (part1-solution)
  (sort ["a" "c" "b"])
  (parse-step "Step C must be finished before step A can begin."))