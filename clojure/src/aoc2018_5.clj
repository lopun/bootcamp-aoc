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
  [s]
  (= s (string/upper-case s)))

(defn lower-case?
  [s]
  (= s (string/lower-case s)))

(defn last-string
  [s]
  (str (last s)))
 
(defn remove-last [str]
  (.substring (java.lang.String. str) 0 (- (count str) 1)))

(defn react?
  [currenty-polymer next-unit]
  (let [compare-operator (if (upper-case? next-unit) string/lower-case string/upper-case)]
    (= (last-string currenty-polymer) (compare-operator next-unit))))

(defn let-polymer-react
  [polymer]
  (->> (string/split polymer #"")
       (reduce (fn [currenty-polymer next-unit]
                 (if (react? currenty-polymer next-unit)
                   (remove-last currenty-polymer)
                   (str currenty-polymer next-unit))))))
  
(defn part1-solution
  []
  (->> (read-file-as-string "resources/aoc2018_5.txt")
       let-polymer-react
       count))

(defn get-all-unit
  []
  (map char (range (int \a) (inc (int \z)))))


(comment
  (map identity "abcde")
  (str "a" "b")
  (reduce str "abcde")
  (part1-solution)
  (remove-last "abc")
  (let-polymer-react  "abcCdDBe")
  (get-all-unit)
  (last "abcde"))
