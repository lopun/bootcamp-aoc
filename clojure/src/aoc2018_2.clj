(ns aoc2018-2)

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


;; #################################
;; ###        Refactoring        ###
;; #################################


;; 파일을 라인별로 분리된 리스트로 리턴해주는 함수입니다.
(defn read_file_as_list []
  (clojure.string/split (slurp "resources/aoc2018_2_1.txt") #"\n"))

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
  (if (some (fn [[key, value]] (= value frequency)) frequency_map) 1 0))

;; 문제의 요구사항에 특화된 함수입니다. 2번, 3번 나타난 문자가 있는지 여부를 확인하고 이들의 SUM 값을 구합니다.
;; ex> (sum_two_and_three {"a" 2, "b" 3}) => 2
;; ex> (sum_two_and_three {"a" 2, "b" 2}) => 1
;; ex> (sum_two_and_three {"a" 3, "b" 3}) => 1
;; ex> (sum_two_and_three {"a" 1, "b" 4}) => 0
(defn sum_two_and_three_frequency [frequency_map]
  (+ (check_frequency_existence frequency_map 2) (check_frequency_existence frequency_map 3)))

;; read_file_as_list 함수로 읽어들인 input_list의 각 라인들을 돌면서 2번, 3번 나타난 문자가 있는지를 확인한 후 이들의 합산값을 구하는 함수입니다.
;; 전체 로직이 담긴 main 함수라고 보시면 됩니다.
(defn map_and_reduce_sum_frequency [input_list]
  (reduce + (map (fn [line] (sum_two_and_three_frequency (count_frequencies line))) input_list)))

(comment
  (+ 1 1)
  (map_and_reduce_sum_frequency (read_file_as_list))
  (count_frequencies "aabbccc")
  (check_frequency_existence (count_frequencies (get (read_file_as_list) 1)) 3)
  (check_frequency_existence (count_frequencies (get (read_file_as_list) 1)) 2)
  (sum_two_and_three_frequency (count_frequencies (get (read_file_as_list) 0)))
  (map (fn [line] line) (read_file_as_list))
  (map (fn [line] (count_frequencies line)) (read_file_as_list))
  (map (fn [line] (sum_two_and_three_frequency (count_frequencies line))) (read_file_as_list))
  (map (fn [line] (sum_two_and_three_frequency (count_frequencies line))) (read_file_as_list))
  (reduce + (map (fn [line] (sum_two_and_three_frequency (count_frequencies line))) (read_file_as_list)))
  (into [] (count_frequencies (get (read_file_as_list) 1)))
  (read_file_as_list)
  (count_frequencies (get (read_file_as_list) 1)))
