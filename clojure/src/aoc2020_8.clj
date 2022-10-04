(ns aoc2020_8
  (:require [clojure.string :as string]
            [file-util :refer [read-file-as-list]]))

(defn parse-boot-code
  [boot-code-str]
  (->> boot-code-str
       (re-find #"(acc|jmp|nop) ([+-]\d+)")
       ((fn [[_ op arg]] {:op op :arg (Integer/parseInt arg)}))))

(defn iterator
  [{:keys [index acc visited-set boot-codes]}]
  (if (>= index (count boot-codes))
    {:index index :acc acc :visited-set visited-set :visited false :terminated true :boot-codes boot-codes}
    (let [{:keys [op arg]} (nth boot-codes index)
          visited (visited-set index)]
      (if visited
        {:index index :acc acc :visited-set visited-set :visited visited :terminated false :boot-codes boot-codes}
        (case op
          "nop" {:index (inc index) :acc acc :visited-set (conj visited-set index) :visited visited :terminated false :boot-codes boot-codes}
          "jmp" {:index (+ index arg) :acc acc :visited-set (conj visited-set index) :visited visited :terminated false :boot-codes boot-codes}
          "acc" {:index (inc index) :acc (+ acc arg) :visited-set (conj visited-set index) :visited visited :terminated false :boot-codes boot-codes})))))

(defn find-first-visited
  [boot-codes]
  (->> boot-codes
       (#(iterate iterator {:index 0 :acc 0 :visited-set #{} :visited false :boot-codes %}))
       (drop-while #(not (:visited %)))
       first))

(defn find-first-visited-or-terminated
  [boot-codes]
  (->> boot-codes
       (#(iterate iterator {:index 0 :acc 0 :visited-set #{} :visited false :boot-codes %}))
       (drop-while #(not (or (:visited %) (:terminated %))))
       first))

(defn part1-solution
  []
  (->> (read-file-as-list "aoc2020_8.txt")
       (map parse-boot-code)
       find-first-visited
       :acc))

(defn toggle-jmp-nop
  [op]
  (case op
    "nop" "jmp"
    "jmp" "nop"
    "acc" "acc"))

(defn apply-toggle-and-get-result-for-all-boot-codes
  [boot-codes]
  (for [index (range 0 (count boot-codes))]
    (find-first-visited-or-terminated (update-in boot-codes [index :op] toggle-jmp-nop))))

(defn part2-solution
  []
  (->> (read-file-as-list "aoc2020_8.txt")
       (map parse-boot-code)
       (into [])
       apply-toggle-and-get-result-for-all-boot-codes
       (filter :terminated)
       first
       :acc))

(comment
  (re-matches #"(acc|jmp|nop) ([+-]\d+)" "acc +1")
  (part1-solution)
  (part2-solution)
  (toggle-jmp-nop "acc")
  (for [index (range 0 3)]
    (println index))
    (println [{:op "nop"} {:op "nop"} {:op "nop"}])
  (parse-boot-code "acc +1")
  )
