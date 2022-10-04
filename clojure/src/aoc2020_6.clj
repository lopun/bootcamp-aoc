(ns aoc2020_6
  (:require [clojure.string :as string]
            [file-util :refer [read-file]]
            [clojure.spec.alpha :as s]))

(defn group-people
  [text]
  (->> (string/split text #"\n\n")
       (map #(string/split % #"\n"))))

(defn into-answer-set
  [answers]
  (->> answers
       (mapcat #(string/split % #""))
       (into #{})))

(defn count-answers-everyone-answered-yes
  [answers]
  (let [people-count (count answers)]
    (->> answers
         (mapcat #(string/split % #"")) ;; mapcat 하는 부분 두가지 함수에서 중복된다. 따로 빼기
         frequencies
         vals
         (filter #(= % people-count))
         count)))

(defn part1-solution
  []
  (->> (read-file "aoc2020_6.txt")
       group-people
       (mapcat into-answer-set)
       count))

(defn part2-solution
  []
  (->> (read-file "aoc2020_6.txt")
       group-people
       (map count-answers-everyone-answered-yes)
       (apply +))) ;; reduce + 대신 apply +


(comment
  (reduce + #{1 2})
  (part1-solution)
  (part2-solution)
  (get [1 2] 1)
  (some? [1 2 nil])
  (->> (read-file-as-list "aoc2020_4.txt")
       group-password-text
       (map parse-passport))
  (->> (read-file-as-list "aoc2020_4.txt")
       group-password-text
       (map parse-passport))
  (Long/parseLong (matching-pattern-or-nil #"pid:(\d+)" "hgt:163in hcl:z pid:8261494508 cid:136 ecl:grn eyr:1958 byr:2030 iyr:1991"))
  (parse-passport  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
  (parse-passport  "hgt:163in hcl:z pid:8261494508 cid:136 ecl:grn eyr:1958 byr:2030 iyr:1991")
  (matching-pattern-or-nil #"hcl:#([0-9a-f]{6})" "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
  (matching-pattern-or-nil #"hcl:#([0-9a-f]{6})$" "hcl:#fffffd")
  (contains? ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"] "amb")
  (Long/parseLong "8261494500"))
  