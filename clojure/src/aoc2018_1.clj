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
  (->> (read-file-as-list  "resources/aoc2018_1_1.txt")
       convert-str-list-to-int-list
       (reduce +)))

;; (defn find-some
;;   ([input-seq
;;     sum-value
;;     sum-occurrence-map]
;;    (lazy-seq
;;     (let [new-sum-value (+ (first input-seq) sum-value)]
;;       (when-let [exists (not (get sum-occurrence-map new-sum-value))]
;;         (println new-sum-value exists)
;;         (if exists new-sum-value
;;             (find-some (rest input-seq) new-sum-value (assoc sum-occurrence-map new-sum-value true))))))))

(defn find-first-duplicate
  [sum-seq]
  (->> (distinct sum-seq)
       (map (fn [original-num distinct-num] (when-not (= original-num distinct-num) original-num)) sum-seq)
       (filter some?) ; some? keep -> map & filter some 을 대체할 수 있다
       first))

(defn part2-solution-seq-way []
  (->> (read-file-as-list "resources/aoc2018_1_2.txt")
       (map #(Integer/parseInt %))
       cycle
       (reductions + 0)
       find-first-duplicate))

(comment
  (+ 1 1)
  (read-file-as-list  "resources/aoc2018_1_2.txt")
  (part2-solution-seq-way)
  (let [input-seq '(1 2 3)]
    (println (first input-seq))
    (println (first input-seq)))
  #_(find-some '(3 3 4 -2 -4 3 3 4 -2 -4) 0 {})
  #_(find-some '(3 3 4 -2 -4 3 3 4 -2 -4) 0 {})
  (reductions + 0 (cycle [3 3 4 -2 -4]))
  (map (fn [a b] ( println a b )) '(1 2 3 4 5) '(6 7 8 9 10))
  (distinct '(1 2 3 4 5 2 3 4 6 7 3 9))
  (get {} :a)
  (assoc {} :key "value"))
