(ns aoc2020_8
  (:require [clojure.string :as string]
            [file-util :refer [read-file-as-list]]))

(defn parse-boot-code
  [boot-code-str]
  (->> boot-code-str
       (re-find #"(acc|jmp|nop) ([+-]\d+)")
       ((fn [[_ op arg]] {:op (keyword op) :arg (Integer/parseInt arg)}))))

;; 검색해보시기: defrecord
;; (defrecord MyState [index acc visited-set visited terminate]) ; closed-map
(defrecord IteratorState [index acc visited-set terminated])

;; assoc
;; dissoc
;; {:index :acc ...} 같은 map을 반복적으로 사용하지 않고 이런 merge 함수를 구현해서 사용할 수 있다.
;; (defn update-state
;;  [state to-updatei]
;;  (merge state to-update))
;; 리뷰: defrecord 사용해서 리팩토링 해봅시다
(defn iterator
  [boot-codes {:keys [index acc visited-set] :as _iterator-state}]
  (cond
    (>= index (count boot-codes)) (IteratorState. index acc visited-set :out-of-index)
    (visited-set index) (IteratorState. index acc visited-set :same-boot-code)
    :else (let [{:keys [op arg]} (nth boot-codes index)]
            (case op
              :nop (IteratorState. (inc index) acc (conj visited-set index) nil)
              :jmp (IteratorState. (+ index arg) acc (conj visited-set index) nil)
              :acc (IteratorState. (inc index) (+ acc arg) (conj visited-set index) nil)))))


;; (name :nop) ; "nop" <-> keyword
(defn find-first-visited
  [boot-codes] ; 리뷰) 부트코드 분리 -> boot-codes 인자를 partial 함수로 넘기는 작업 진행해보기(상태값이 아니라서 굳이 인자로 계속 넘길 이유가 없다)
  (->> (iterate (partial iterator boot-codes) {:index 0 :acc 0 :visited-set #{} :terminated nil})
       (drop-while #(= (:terminated %) :same-boot-code))
       first))

(defn find-first-visited-or-terminated
  [boot-codes]
  (let [init-state {:index 0
                    :acc 0
                    :visited-set #{}
                    :visited false
                    :boot-codes boot-codes}]
    (->> init-state
         (iterate (partial iterator boot-codes))
         (drop-while #(not (:terminated %)))
         first)))

(defn part1-solution
  []
  (->> (read-file-as-list "aoc2020_8.txt")
       (map parse-boot-code)
       find-first-visited
       :acc))

(defn toggle-jmp-nop
  [op]
  (case op
    :nop :jmp
    :jmp :nop
    :acc :acc))

(defn apply-toggle-and-get-result-for-all-boot-codes
  [boot-codes]
  (for [index (range 0 (count boot-codes))]
    (find-first-visited-or-terminated (update-in boot-codes [index :op] toggle-jmp-nop))))

(defn part2-solution
  []
  (->> (read-file-as-list "aoc2020_8.txt")
       (map parse-boot-code)
       (into [])
       apply-toggle-and-get-result-for-all-boot-codes
       (filter #(= (:terminated %) :out-of-index))
       first
       :acc))

(comment
  (re-matches #"(acc|jmp|nop) ([+-]\d+)" "acc +1")
  (part1-solution)
  (part2-solution)
  (toggle-jmp-nop "acc")
  (nth [1 2 3] 100)
  (for [index (range 0 3)]
    (println index))
  (println [{:op "nop"} {:op "nop"} {:op "nop"}])
  (parse-boot-code "acc +1"))
