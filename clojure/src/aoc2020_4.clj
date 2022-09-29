(ns aoc2020_4
  (:require [clojure.string :as string]
            [file-util :refer [read-file-as-list]]))

(defn matching-pattern-or-nil
  [pattern text]
  (let [found-pattern (re-find pattern text)]
    (when found-pattern (second found-pattern))))

(defn group-password-text
  [seperated-texts]
  (->> seperated-texts
       (string/join " ")
       (#(string/split % #"  "))))

(defn if-exists-parse-long
  [text]
  (when text (Long/parseLong text)))

(defn parse-passport
  [passport-text]
  {:byr (matching-pattern-or-nil #"byr:(\w+)" passport-text)
   :iyr (matching-pattern-or-nil #"iyr:(\w+)" passport-text)
   :eyr (matching-pattern-or-nil #"eyr:(\w+)" passport-text)
   :hgt (matching-pattern-or-nil #"hgt:(\w+)" passport-text)
   :hcl (matching-pattern-or-nil #"hcl:(#\w+)" passport-text)
   :ecl (matching-pattern-or-nil #"ecl:(\w+)" passport-text)
   :pid (matching-pattern-or-nil #"pid:(\w+)" passport-text)
   :cid (matching-pattern-or-nil #"cid:(\w+)" passport-text)})

(defn strictly-parse-passport
  [passport-text]
  {:byr (if-exists-parse-long (matching-pattern-or-nil #"byr:(\d+)" passport-text))
   :iyr (if-exists-parse-long (matching-pattern-or-nil #"iyr:(\d+)" passport-text))
   :eyr (if-exists-parse-long (matching-pattern-or-nil #"eyr:(\d+)" passport-text))
   :hgt (let [hgt-text (matching-pattern-or-nil #"hgt:(\w+)" passport-text)]
          (when hgt-text {:num (Long/parseLong (matching-pattern-or-nil #"(\d+)" hgt-text))
                          :is-inch (when (matching-pattern-or-nil #"(\d+in)" hgt-text) true)
                          :is-cm (when (matching-pattern-or-nil #"(\d+cm)" hgt-text) true)}))
   :hcl (matching-pattern-or-nil #"hcl:(#\w+)" passport-text)
   :ecl (matching-pattern-or-nil #"ecl:(\w+)" passport-text)
   :pid (matching-pattern-or-nil #"pid:(\w+)" passport-text)
   :cid (if-exists-parse-long (matching-pattern-or-nil #"cid:(\d+)" passport-text))})

(defn validate-passport
  [passport]
  (let [required-fields [:byr :iyr :eyr :hgt :hcl :ecl :pid]
        required-fields-count (count required-fields)]
    (->> required-fields
         (keep #(% passport))
         count
         (= required-fields-count))))

(defn strictly-validate-passport
  [passport]
  (let [{:keys [byr iyr eyr hgt hcl ecl pid]} passport]
    (if (validate-passport passport)
      (println ecl (some #(= ecl %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])))
    (when (and
           (validate-passport passport)
           ;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
           (>= byr 1920)
           (<= byr 2002)
           ;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
           (>= iyr 2010)
           (<= iyr 2020)
           ;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
           (>= eyr 2020)
           (<= eyr 2030)
           ;; hgt (Height) - a number followed by either cm or in:
           ;; If cm, the number must be at least 150 and at most 193.
           ;; If in, the number must be at least 59 and at most 76.
           (cond (:is-inch hgt) (and (>= (:num hgt) 59) (<= (:num hgt) 76))
                 (:is-cm hgt) (and (>= (:num hgt) 150) (<= (:num hgt) 193))
                 :else nil)
           ;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
           (matching-pattern-or-nil #"#([0-9a-f]{6})$" hcl)
           ;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
           (some #(= ecl %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
           ;; pid (Passport ID) - a nine-digit number, including leading zeroes.
           (matching-pattern-or-nil #"(\d{9})$" pid)) true)))
    

(defn part1-solution
  []
  (->> (read-file-as-list "aoc2020_4.txt")
       group-password-text
       (map parse-passport)
       (filter validate-passport)
       count))

(defn part2-solution
  []
  (->> (read-file-as-list "aoc2020_4.txt")
       group-password-text
       (map strictly-parse-passport)
       (filter strictly-validate-passport)
       count))

(comment
  (reduce + #{1 2})
  (part1-solution)
  (part2-solution)
  (get [1 2] 1)
  (some? [1 2 nil])
(->> (read-file-as-list "aoc2020_4.txt")
       group-password-text
       (map parse-passport)
       )
  (->> (read-file-as-list "aoc2020_4.txt")
       group-password-text
       (map parse-passport))
  (Long/parseLong (matching-pattern-or-nil #"pid:(\d+)" "hgt:163in hcl:z pid:8261494508 cid:136 ecl:grn eyr:1958 byr:2030 iyr:1991"))
  (parse-passport  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
  (parse-passport  "hgt:163in hcl:z pid:8261494508 cid:136 ecl:grn eyr:1958 byr:2030 iyr:1991")
  (matching-pattern-or-nil #"hcl:#([0-9a-f]{6})" "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
  (matching-pattern-or-nil #"hcl:#([0-9a-f]{6})$" "hcl:#fffffd")
  (contains? ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"] "amb")
  (and true true true)
  (and true true false)
  (Long/parseLong "8261494500"))
