(ns aoc2020_4
  (:require [clojure.string :as string]
            [file-util :refer [read-file-as-list]]
            [clojure.spec.alpha :as s]))

(defn matching-pattern-or-nil
  [pattern text]
  (when-let [found-pattern (re-find pattern text)] ;; when-let으로 리펙토링 가능
    (second found-pattern)))

(defn group-password-text
  [seperated-texts]
  (->> seperated-texts
       (string/join " ")
       (#(string/split % #"  "))))

(defn if-exists-parse-long
  [text]
  (when text (parse-long text))) ; clojure 1.10.xx -> 1.11.xx parse-long

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
          (when hgt-text {:num (parse-long (matching-pattern-or-nil #"(\d+)" hgt-text))
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

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
(s/def :acct/byr (s/and #(>= % 1920) #(<= % 2002))) ;; int-in으로 리펙토링 가능 (https://clojuredocs.org/clojure.spec.alpha/int-in)
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(s/def :acct/iyr (s/and #(>= % 2010) #(<= % 2020)))
;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(s/def :acct/eyr (s/and #(>= % 2020) #(<= % 2030)))
;; hgt (Height) - a number followed by either cm or in:
;; If cm, the number must be at least 150 and at most 193.
;; If in, the number must be at least 59 and at most 76.
(s/def :acct/hgt (fn [hgt]
                   (cond (:is-inch hgt) (s/and (>= (:num hgt) 59) (<= (:num hgt) 76)) ;; (<= 59 hgt 76) 으로 리펙토링 가능
                         (:is-cm hgt) (s/and (>= (:num hgt) 150) (<= (:num hgt) 193)) ;; (+ 1 1 2 3 4 5 6 6 7 8 9 0 0)
                         :else nil)))

;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
(s/def :acct/hcl #(matching-pattern-or-nil #"#([0-9a-f]{6})$" %))
;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(s/def :acct/ecl (fn [ecl]
                   (some #(= ecl %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])))
;; pid (Passport ID) - a nine-digit number, including leading zeroes.
(s/def :acct/pid #(matching-pattern-or-nil #"(\d{9})$" %))
(s/def :acct/cid (s/nilable int?))
(s/def :acct/passport (s/keys :req [:acct/byr :acct/iyr :acct/eyr :acct/hgt :acct/hcl :acct/ecl :acct/pid] ;; req-un, opt-un으로 리펙토링
                              :opt [:acct/cid]))

;; unqualified...
;; qualified...
;; (clojure.core/keep) -> qualified
;; (keep) -> unqualified

(defn strictly-validate-passport
  [passport]
  (let [{:keys [byr iyr eyr hgt hcl ecl pid cid]} passport]
    (s/valid? :acct/passport {:acct/byr byr
                              :acct/iyr iyr
                              :acct/eyr eyr
                              :acct/hgt hgt
                              :acct/hcl hcl
                              :acct/ecl ecl
                              :acct/pid pid
                              :acct/cid cid})))
    

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
       (filter validate-passport)
       (filter strictly-validate-passport)
       count))

(comment
  (reduce + #{1 2})
  (part1-solution)
  (part2-solution)
  (strictly-validate-passport {:byr 1980, :iyr 2012, :eyr 2030, :hgt {:num 74, :is-inch true, :is-cm nil}, :hcl "#623a2f", :ecl "grn", :pid "087499704", :cid nil})
  (s/valid? :acct/passport ((:acct/byr 2003)
                            #_(:acct/iyr iyr)
                            #_(:acct/eyr eyr)
                            #_(:acct/hgt hgt)
                            #_(:acct/hcl hcl)
                            #_(:acct/ecl ecl)
                            #_(:acct/pid pid)
                            #_(:acct/cid cid)))
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
  (Long/parseLong "8261494500")
  )

;; TODO
;; spec 읽어서 해당 라이브러리 기반으로 리펙토링 다시 진행해보기. (https://johngrib.github.io/wiki/clojure/guide/spec/)
;; 이 링크도 읽어보기 -> https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
;; 위 링크 2개 읽고 4번문제 마저 풀어도 좋을 것 같다
;; 2020-6번 & 2018-7번 문제 마저 풀이 해야 한다. (til tomorrow)
;; -> 18/7번이 제일 어렵고 20/6번은 상대적으로 쉬운데 18/7번 풀이의 선행이 된다.
;; looping & 상태 관리가 빡세다. reduce를 써야 할 수도 있는데...
;; iterate / drop-while / take-while
;; 상태를 관리해야하고, 계속 반복 작업해야한다..
;; (comment
;;   (->> init-state ;; Parse
;;        iterate ;; core함수 ; process
;;        stop ;; drop-while/take-while ;; core함수
;;        agg ; agg
;;        ans)) ; print
