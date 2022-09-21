(ns aoc2018-1)

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력


;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
;; Tips.) loop recursive, cycle, repeat maybe?

;; +1, -200, ...

;; (defn apply-arithmetic-expr [prev-num expr]
;;   (cond (= (get expr 0) \+) (+ prev-num (Integer/parseInt (subs expr 1)))
;;         (= (get expr 0) \-) (- prev-num (Integer/parseInt (subs expr 1)))
;;         (= (get expr 0) \/) (/ prev-num (Integer/parseInt (subs expr 1)))
;;         (= (get expr 0) \*) (* prev-num (Integer/parseInt (subs expr 1)))))

(defn read-file-as-list [input_src]
  (clojure.string/split (slurp input_src) #"\n"))

(defn convert-str-list-to-int-list
  "
  input: [\"+1\" \"-1\"]
  output: [1 -1]
  "
  [str-list]
  (map #(Integer/parseInt %) str-list))

(defn part1-solution []
   (reduce + (convert-str-list-to-int-list (read-file-as-list  "resources/aoc2018_1_1.txt"))))

;; (defn part2-solution []
;;   (let [
;;         input-list (read-file-as-list)
;;         input-count (count input-list)
;;       ] 
;;       (->> (range (- input-count 1)) 
;;         (map (fn [index]
;;           (reduce + (convert-str-list-to-int-list (take (+ index 1) input-list)))))
;;            )))

(defn find-earliest-2nd-occurrence-sum-value [input-list]
  (loop [current-input-list input-list
         sum-value 0
         sum-occurrence-map {}]
    (let [new-sum-value (+ (first current-input-list) sum-value)]
      (if (or (get sum-occurrence-map new-sum-value) (= (count current-input-list) 0))
        new-sum-value
        (recur (rest current-input-list) new-sum-value (assoc sum-occurrence-map new-sum-value true))))))

(defn part2-solution []
  (->> (read-file-as-list "resources/aoc2018_1_2.txt")
       (map #(Integer/parseInt %))
       (find-earliest-2nd-occurrence-sum-value)))


(comment 
    (+ 1 1)
    (read-file-as-list  "resources/aoc2018_1_2.txt")
    (part2-solution)
    (assoc {} :key "value"))
  

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
