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

(defn instructions
  [steps]
  (->> steps
       (group-by :to)
       (fmap #(map :from %))))

(defn all-steps
  [steps]
  (into #{} (concat (map :to steps) (map :from steps))))

(defn get-start-steps
  [{:keys [all-steps instructions]}]
  (sort (set/difference all-steps
                        (into #{} (keys instructions)))))

(defn to-manual
  [steps]
  {:instructions (instructions steps) :all-steps (all-steps steps)})

(defn renew-available
  [available answer {:keys [all-steps instructions] :as _manual}]
  (let [completed-set (into #{} answer)
        not-completed-set (set/difference all-steps completed-set)]
    (->> not-completed-set
         (filter #(empty? (set/difference (into #{} (instructions %)) completed-set)))
         (concat available)
         distinct
         sort)))

(defn find-steps-for-one-elf [manual]
  (loop [available (get-start-steps manual)
         answer []]
    (if (empty? available)
      answer
      (let [result (conj answer (first available))]
        (recur (renew-available (rest available) result manual) result)))))

(defn part1-solution
  []
  (->> (read-file-as-list "aoc2018_7.txt")
       (map parse-step)
       to-manual
       find-steps-for-one-elf
       (apply str)))

(defn find-steps-for-elves-with-sec
  [manual elves-count secs]
  (loop [available (get-start-steps manual)
         answer []
         count 0
         elves []]
    (if (and (empty? available) (empty? elves))
      count)))


(comment
  (part1-solution)
  (sort ["a" "c" "b"])
  (parse-step "Step C must be finished before step A can begin."))

;; 2020/8 선행으로 먼저 풀이 시작 (part2 참고)