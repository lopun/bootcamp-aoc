(ns aoc2018-2)

;; ==================================================================================
;; ====== PART 1 START ===== PART 1 START ===== PART 1 START ===== PART 1 START =====
;; ==================================================================================

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1) = 1 + 1
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1) = 1 + 0 = 1
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2) = 1 + 0 = 1
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2) = 1 + 0 = 1
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

;; 파일을 라인별로 분리된 리스트로 리턴해주는 함수입니다.
(defn read_file_as_list [input_src]
  (clojure.string/split (slurp input_src) #"\n"))

;; 입력 String에 대해서 각 문자들이 몇번씩 나타났는지 Frequency Map을 리턴해주는 함수입니다.
;; ex> (count_frequencies "aabbccc") => {"a" 2, "b" 2, "c" 3}
(defn count_frequencies [str]
  (frequencies (clojure.string/split str #"")))

;; count_ferquencies 함수에서 받은 FrequencyMap에서 frequency가 존재하는지를 확인해주는 함수입니다.
;; 추후에 이 값들을 합쳐야 하는 요구사항이 있기 때문에 편의성을 위해서 true/false가 아닌 1/0을 리턴하도록 함수를 구성했습니다.
;; ex> (check_frequency_existence {"a" 2, "b" 2, "c" 3} 1) => 0
;; ex> (check_frequency_existence {"a" 2, "b" 2, "c" 3} 2) => 1
;; ex> (check_frequency_existence {"a" 2, "b" 2, "c" 3} 3) => 1
(defn check_frequency_existence [frequency_map, frequency]
  (if (some (fn [[_, value]] (= value frequency)) frequency_map) 1 0))
  
(defn part1_solution []
  (->> (read_file_as_list "resources/aoc2018_2_1.txt")
    (map count_frequencies)
    (map (fn [frequency_map] [(check_frequency_existence frequency_map 2) (check_frequency_existence frequency_map 3)]))
    (reduce (fn [[prev_two prev_three], [new_two new_three]] [(+ prev_two new_two) (+ prev_three new_three)]))
    (apply *)
  ))

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

;; 특정 인덱스에 있는 값을 제거한 string을 리턴하는 함수입니다.
;; ex> (get_index_removed_str "0123456" 2) => "013456"
(defn get_index_removed_str [index text]
  (str (subs text 0 index) (subs text (+ index 1))))

;; text_list에서 한개 이상 있는 문자를 찾아서 리턴하는 함수입니다.
;; ex> (find_duplicate_item_in_list ["a" "b" "a" "c"]) => "a"
(defn find_duplicate_item_in_list [text_list]
  (->> text_list
       (frequencies)
       (filter (fn [[_ freq]] (> freq 1)))
       (first)
       (first)
       ))

(defn part2_solution []
  (let [
        input_list (read_file_as_list "resources/aoc2018_2_2.txt") ;; input 파일
        text_length (count (first input_list)) ;; input 파일의 첫번째 라인의 글자수
      ] 
    (->> (range text_length) 
      (map (fn [index] ;; 텍스트 길이만큼 iterate
         (->> input_list ;; input들을 돌면서 특정 index를 제거한 후 겹치는 문자가 있는지 확인하는 로직
            (map (partial get_index_removed_str index))
            (find_duplicate_item_in_list))))
      (filter some?) ;; 겹치는 문자가 있었다면
      (first) ;; 제일 먼저 겹치는 문자를 리턴한다
    )))


(comment
  (+ 1 1)
  (part1_solution)
  (part2_solution)
  ((partial get_index_removed_str 3) "asdifoas")
  (find_duplicate_item_in_list (map (partial get_index_removed_str 3) ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"]))
  (count_frequencies "aabbccc")
)