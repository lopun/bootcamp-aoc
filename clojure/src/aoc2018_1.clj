(ns aoc2018-1)

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력


;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

;; +1, -200, ...

;; (defn apply_arithmetic_expr [prev_num expr]
;;   (cond (= (get expr 0) \+) (+ prev_num (Integer/parseInt (subs expr 1)))
;;         (= (get expr 0) \-) (- prev_num (Integer/parseInt (subs expr 1)))
;;         (= (get expr 0) \/) (/ prev_num (Integer/parseInt (subs expr 1)))
;;         (= (get expr 0) \*) (* prev_num (Integer/parseInt (subs expr 1)))))

(defn read_file_as_list []
  (clojure.string/split (slurp "resources/aoc2018_1.txt") #"\n"))

(defn convert_str_list_to_int_list [str_list]
  (map (fn [x] (Integer/parseInt x)) str_list))

(comment 
    (+ 1 1)
    (read_file_as_list)
    (convert_str_list_to_int_list (read_file_as_list))
    (reduce + (convert_str_list_to_int_list (read_file_as_list)))
  )

(defn main []
   (reduce + (convert_str_list_to_int_list (read_file_as_list))))


;; (defn f [y]
;; (let [a (g x)] 
;;    ... )
;;  const a = g(x);

;;  (h a)
;;  ()
;;  ()
;;  ()) <-- return
;; 1. ["+1" "+1" "-1" ...] (O)
;; 2. [1 1 -1 100 -5 ...]
;; 3. ... sum


;; PPAP : Parse Process Aggregate Print
