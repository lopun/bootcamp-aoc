(ns aoc2018-2
 (:require [clojure.string :as string]))

;; ==================================================================================
;; ====== PART 1 START ===== PART 1 START ===== PART 1 START ===== PART 1 START =====
;; ==================================================================================

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

;; 
(defn read-file-as-list "파일을 라인별로 분리된 리스트로 리턴해주는 함수입니다." [input-src]
  (clojure.string/split (slurp input-src) #"\n"))

;; 입력 String에 대해서 각 문자들이 몇번씩 나타났는지 Frequency Map을 리턴해주는 함수입니다.
;; ex> (count-frequencies "aabbccc") => {"a" 2, "b" 2, "c" 3}
(defn count-frequencies [str]
  (frequencies (string/split str #"")))
(comment
 (frequencies "aabbccc"))
;; count-ferquencies 함수에서 받은 FrequencyMap에서 frequency가 존재하는지를 확인해주는 함수입니다.
;; 추후에 이 값들을 합쳐야 하는 요구사항이 있기 때문에 편의성을 위해서 true/false가 아닌 1/0을 리턴하도록 함수를 구성했습니다.
;; ex> (check-frequency-existence {"a" 2, "b" 2, "c" 3} 1) => 0
;; ex> (check-frequency-existence {"a" 2, "b" 2, "c" 3} 2) => 1
;; ex> (check-frequency-existence {"a" 2, "b" 2, "c" 3} 3) => 1
(defn check-frequency-existence [frequency-map frequency]
  (if (some (fn [[- value]] (= value frequency)) frequency-map) 1 0))

; vals
; 2 3
; partial (currying) https://clojuredocs.org/clojure.core/partial
;; format
(defn part1-solution []
  (->> (read-file-as-list "resources/aoc2018_2_1.txt")
       (map frequencies)
       (map (fn [frequency-map] [(check-frequency-existence frequency-map 2) (check-frequency-existence frequency-map 3)]))
       (reduce (fn [[prev-two prev-three], [new-two new-three]] [(+ prev-two new-two) (+ prev-three new-three)]))
       (apply *)))

;; ==================================================================================
;; ====== PART 2 START ===== PART 2 START ===== PART 2 START ===== PART 2 START =====
;; ==================================================================================

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

(defn get-index-removed-list [text-list index]
  (->> text-list
       (map #(str (subs % 0 index) (subs % (+ index 1))))
       frequencies
       (filter (fn [[- freq]] (> freq 1)))
       ffirst))

(defn part2-solution []
  (let [input-list (read-file-as-list "resources/aoc2018_2_2.txt")]
    (loop [index 0]
      (let [result (get-index-removed-list input-list index)]
        (if result result (recur (+ index 1)))))))


(comment
  (+ 1 1)
  (part1-solution)
  (part2-solution)
  (count-frequencies "aabbccc"))