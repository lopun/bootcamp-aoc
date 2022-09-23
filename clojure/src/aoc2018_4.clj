(ns aoc2018_4
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clj-time.core :as t]
            [clj-time.format :as f]))

(defn read-file-as-list "파일을 라인별로 분리된 리스트로 리턴해주는 함수입니다." [input-src]
  (clojure.string/split (slurp input-src) #"\n"))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(def time-formatter (f/formatter "yyyy-MM-dd HH:mm"))

(defn parse-and-get-sleep-info
  "
  텍스트 라인을 읽어서  정보를 리턴해주는 함수입니다.
  ex1)
  input: [1518-11-05 00:03] Guard #99 begins shift
  output: {:minute 3}
  ex2)
  input: [1518-11-04 00:36] falls asleep
  output: {}
  ex2)
  input: [1518-11-05 00:55] wakes up
  output: 
  "
  [line]
  (let [minute (Integer/parseInt (first (string/split (last (string/split line #"\:")) #"\]")))
        type (cond (string/starts-with? (last (string/split line #"] ")) "Guard") :guard
                   (string/starts-with? (last (string/split line #"] ")) "falls") :start
                   (string/starts-with? (last (string/split line #"] ")) "wakes") :end)
        id (if (= type :guard) (first (string/split (last (string/split line #"\#")) #" begins shift")))]
    {:minute minute :type type :id id}))

(defn track-sleep-for-each-guard
  [grouped-list sleep-info]
  (cond (= (get sleep-info :type) :guard) (conj grouped-list {:id (get sleep-info :id) :sleep-info []})
        (= (get sleep-info :type) :start) (update-in grouped-list [(- (count grouped-list) 1) :sleep-info] conj (get sleep-info :minute))
        (= (get sleep-info :type) :end) (update-in grouped-list [(- (count grouped-list) 1) :sleep-info] conj (get sleep-info :minute))))

(defn get-sleep-minutes
  [sleep-info-group]
  (->> sleep-info-group
       (map :sleep-info)
       flatten
       (partition 2)
       (map #(range (first %) (last %)))
       flatten))

(defn group-by-guard-and-collect-info
  [sleep-for-each-guard]
  (->> sleep-for-each-guard
       (group-by :id)
       (map (fn [[id sleep-info-group]] {:id id :sleep-minutes (get-sleep-minutes sleep-info-group)}))))

(defn part1-solution
  []
  (->> (read-file-as-list "resources/aoc2018_4.txt")
       (map parse-and-get-sleep-info)
       (reduce track-sleep-for-each-guard [])
       group-by-guard-and-collect-info))

(comment
  (read-file-as-list "resources/aoc2018_4.txt")
  (t/in-minutes (t/interval (t/date-time 2022 9 23 2 00) (t/now)))
  ;; (t/in-years (t/interval (f/parse custom-formatter "1518-11-05 00:55") (t/now)))
  (f/show-formatters)
  (f/parse "1518-11-05 00:55")
  (f/parse (f/formatters :basic-date-time) "1518-11-05 00:55")
  (f/parse time-formatter "1518-11-05 00:55")
  (conj [1] [2 3])
  (parse-and-get-sleep-info "[1518-11-05 00:03] Guard #99 begins shift")
  (parse-and-get-sleep-info "[1518-11-04 00:36] falls asleep")
  (parse-and-get-sleep-info "[1518-11-05 00:55] wakes up")
  (partition 2 [1 2 3 4 5 6])
  (range 0 1)
  (part1-solution)
  )