(ns aoc2018_5
  (:require [clojure.string :as string]))

(defn read-file-as-string "파일을 라인별로 분리된 리스트로 리턴해주는 함수입니다." [input-src]
  (slurp input-src))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn upper-case?
  "
  문자가 대문자인지 확인해주는 함수입니다.
  ex1)
  input: A
  output: true
  ex2)
  input: a
  output: false 
  "
  [s]
  (= s (string/upper-case s)))

(defn lower-case?
  "
  문자가 소문자인지 확인해주는 함수입니다.
  ex1)
  input: A
  output: false
  ex2)
  input: a
  output: true
  "
  [s]
  (= s (string/lower-case s)))

(defn last-string
  "
  마지막 문자를 가져와주는 함수입니다. Character이 아닌 String 타입으로 가져옵니다.
  ex)
  input: abcde
  output: e
  "
  [s]
  (str (last s)))

(defn remove-last
  "
  마지막 문자가 제거된 문자열을 리턴하는 함수입니다.
  ex)
  input: abcde
  output: abcd
  "
  [str]
  (.substring (java.lang.String. str) 0 (- (count str) 1)))

(defn can-react?
  "
  현재 polymer와 새로 들어온 unit이 반응할 수 있는지를 확인해주는 함수입니다.
  ex1) d와 D가 반응
  input: abcd D
  output: true
  ex2) d와 E는 반응불가
  input: abcd E
  output: false
  "
  [currenty-polymer next-unit]
  (let [compare-operator (if (upper-case? next-unit) string/lower-case string/upper-case)]
    (= (last-string currenty-polymer) (compare-operator next-unit))))

(defn let-polymer-react
  "
  polymer를 반응시키는 함수입니다. 반응할 수 있는 모든 unit은 제거됩니다.
  ex) dabAcCaCBAcCcaDA => dabCBAcaDA
  input: dabAcCaCBAcCcaDA
  output: dabCBAcaDA
  "
  [polymer]
  (->> (string/split polymer #"")
       (reduce (fn [currenty-polymer next-unit]
                 (if (can-react? currenty-polymer next-unit)
                   (remove-last currenty-polymer)
                   (str currenty-polymer next-unit))))))

(defn part1-solution
  []
  (->> (read-file-as-string "resources/aoc2018_5.txt")
       let-polymer-react
       count))

(defn get-all-unit
  "
  가능한 모든 unit(a-z)을 리턴하는 함수입니다. 해당 함수에서는 소문자만 리턴합니다.
  ex)
  input:
  output: (\a \b \c ... \z)
  "
  []
  (map char (range (int \a) (inc (int \z)))))

(defn remove-unit-from-polymer
  "
  polymer에서 unit에 해당하는 모든 문자를 제거하는 함수입니다.
  ex)
  input: \a abcdABCD
  output: bcdBCD
  "
  [unit polymer]
  (-> polymer
      (string/replace (string/lower-case unit) "")
      (string/replace (string/upper-case unit) "")))


(comment
  (map identity "abcde")
  (str "a" "b")
  (reduce str "abcde")
  (part1-solution)
  (remove-last "abc")
  (let-polymer-react  "abcCdDBe")
  (remove-unit-from-polymer \a "abAc")
  (get-all-unit)
  (last "abcde"))
  
