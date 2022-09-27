(ns aoc2018_4
  (:require [clojure.string :as string]
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

(def time-formatter 
  "
  nil이나 empty 상태가 아닐 경우 func 함수를 적용시켜주는 함수입니다.
  ex)
  input: [2022-09-07 11:33]
  output: #object[org.joda.time.DateTime 0x7fc9bc1e 1518-11-05T00:55:00.000Z]
  "
  (f/formatter "yyyy-MM-dd HH:mm"))

(defn apply-if-not-empty-or-nil
  "
  nil이나 empty 상태가 아닐 경우 func 함수를 적용시켜주는 함수입니다.
  ex1)
  input: last nil
  output: nil 
  ex2)
  input: last [1 2 3]
  output: 3
  "
  [func list-or-nil]
  (if (and list-or-nil (not-empty list-or-nil)) (func list-or-nil) nil))

(defn apply-if-not-nil
  "
  nil이 아닐 경우 func 함수를 적용시켜주는 함수입니다.
  ex1)
  input: #(Input/parseInt %) nil
  output: nil 
  ex2)
  input: #(Input/parseInt %) '123'
  output: 123
  "
  [func val-or-nil]
  (if val-or-nil (func val-or-nil) nil))

(defn parse-and-get-sleep-info
  "
  라인을 읽어서 가드의 잠자는 정보를 분석해주는 함수입니다.
  ex1)
  input: [1518-11-05 00:03] Guard #99 begins shift
  output: {:time #object[org.joda.time.DateTime ...] :minute 3 :type :guard :id 99}
  ex2)
  input: [1518-11-04 00:36] falls asleep
  output: {:time #object[org.joda.time.DateTime ...] :minute 36 :type :start :id nil}
  ex3)
  input: [1518-11-05 00:55] wakes up
  output: {:time #object[org.joda.time.DateTime ...] :minute 55 :type :end :id nil}
  "
  [line]
  (let [time (f/parse time-formatter (last (re-find #"\[(.+)\]" line)))
        minute (Integer/parseInt (last (re-find #"(\d+):(\d+)" line)))
        type (case (last (re-find #"\[.+\] (\w+)" line))
               "Guard" :guard
               "falls"  :start
               "wakes" :end)
        id (->> (re-find #"#(\d+)" line)
                (apply-if-not-empty-or-nil last)
                (apply-if-not-nil #(Integer/parseInt %)))]
    {:time time :minute minute :type type :id id}))

(defn track-sleep-for-each-guard
  "
  가드별로 잠들기 시작/종료하는 시간들을 그룹핑 해주는 함수입니다. 
  (*Tip. reduce 함수로 적용시키는 sub 함수입니다.)

  ex1)
  input: [] {:time #object[org.joda.time.DateTime ...] :minute 3 :type :guard :id 99}
  output: [{:id 99 :sleep-info []}]
  ex2)
  input: [{:id 99 :sleep-info []}] {:time #object[org.joda.time.DateTime ...] :minute 36 :type :start :id nil}
  output: [{:id 99 :sleep-info [36]}]
  ex3)
  input: [{:id 99 :sleep-info [36]}] {:time #object[org.joda.time.DateTime ...] :minute 55 :type :end :id nil}
  output: [{:id 99 :sleep-info [36 55]}]
  "
  [grouped-list {:keys [type id minute]}]
  ;; cond -> case
  (case type
        :guard (conj grouped-list {:id id :sleep-info []})
        :start (update-in grouped-list [(dec (count grouped-list)) :sleep-info] conj minute)
        :end (update-in grouped-list [(dec (count grouped-list)) :sleep-info] conj minute)))

(defn get-sleep-minutes
  "
  총 잠든 분(minute)들을 구해주는 함수입니다.

  ex) 가드 99번이 1~10, 25~35분까지 잤다는 가정
  input: [{:id 99 :sleep-info [1 10 25 35]}]
  output: (1 2 ... 9 25 26 ... 34)
  "
  [sleep-info-group]
  (->> sleep-info-group
       (mapcat :sleep-info) ;; mapcat
       (partition 2)
       (mapcat (fn [[start, end]] (range start end)))))

(defn get-frequently-sleep-minute-and-times
  "
  가드가 가장 많이 잠든 분(minute)를 리턴해주는 함수입니다.

  ex) 4분에 3번 잠듬
  input: (1 2 3 4 4 4 5 6)
  output: {:minute 4 :times 3}
  "
  [sleep-minutes]
  (->> sleep-minutes
       frequencies
       (sort-by val) ;; max-key 찾아서 수정
       last
       ((fn [[minute, times]] {:minute minute :times times}))))

(defn get-sleep-summary
  "
  가드의 잠든 시간의 정보를 요약해주는 함수입니다.

  ex) 
  input: [99 [{:id 99 :sleep-info [1 10 25 35]}]]
  output: {:id 99, :total-sleep-time 19, :frequently-sleep-minute-and-times {:minute 8, :times 1}}
  "
  [[id sleep-info-group]]
  (let [sleep-minutes (get-sleep-minutes sleep-info-group)]
    {:id id
     :total-sleep-time (count sleep-minutes)
     :frequently-sleep-minute-and-times (get-frequently-sleep-minute-and-times sleep-minutes)}))

(defn group-by-guard-and-get-sleep-summary
  "
  가드별로 잠든 시간 정보들을 받아 각 가드별로 그룹핑 한 후 sleep summary를 가져오는 함수입니다.

  ex)
  input: [{:id 10, :sleep-info [5 25 30 55]} {:id 99, :sleep-info [40 50]} {:id 10, :sleep-info [24 29]} {:id 99, :sleep-info [36 46]} {:id 99, :sleep-info [45 55]}]
  output: ({:id 10, :total-sleep-time 50, :frequently-sleep-minute-and-times {:minute 24, :times 2}} {:id 99, :total-sleep-time 30, :frequently-sleep-minute-and-times {:minute 45, :times 3}})
  "
  [sleep-for-each-guard]
  (->> sleep-for-each-guard
       (group-by :id)
       (filter #(not-empty (get-sleep-minutes (val %))))
       (map get-sleep-summary)))

(defn get-most-total-sleep-time-between-guard
  "
  제일 많이 잠든 가드의 sleep summary를 가져오는 함수입니다.
  (*Tip. group-by-guard-and-get-sleep-summary 함수 다음에 사용됩니다.)
  (*Tip. part1 문제에 사용됩니다.)

  ex) id 10번이 50분, id 99번이 30분을 잤으므로 10번의 정보를 리턴합니다.
  input: ({:id 10, :total-sleep-time 50, :frequently-sleep-minute-and-times {:minute 24, :times 2}} {:id 99, :total-sleep-time 30, :frequently-sleep-minute-and-times {:minute 45, :times 3}})
  output: {:id 10, :total-sleep-time 50, :frequently-sleep-minute-and-times {:minute 24, :times 2}}
  "
  [sleep-summary-for-each-guard]
  (->> sleep-summary-for-each-guard
       (sort-by :total-sleep-time)
       last))

(defn get-most-frequently-sleep-minuite-between-guard
  "
  특정 분(minute)에 제일 많이 잠든 가드의 sleep summary를 가져오는 함수입니다.
  (*Tip. group-by-guard-and-get-sleep-summary 함수 다음에 사용됩니다.)
  (*Tip. part2 문제에 사용됩니다.)

  ex) 특정 분(minute)에 제일 많이 잠든 시간은 45분이므로(총 3번) 해당 시간에 제일 많이 잔 id 99번의 정보를 리턴합니다.
  input: ({:id 10, :total-sleep-time 50, :frequently-sleep-minute-and-times {:minute 24, :times 2}} {:id 99, :total-sleep-time 30, :frequently-sleep-minute-and-times {:minute 45, :times 3}})
  output: {:id 99, :total-sleep-time 30, :frequently-sleep-minute-and-times {:minute 45, :times 3}}
  "
  [sleep-summary-for-each-guard]
  (->> sleep-summary-for-each-guard
       (sort-by #(:times (:frequently-sleep-minute-and-times %)))
       last))

(defn multiply-minute-and-id
  "
  sleep-summary의 id와 minute을 곱합니다.
  (*Tip. part1, part2의 최종 solution을 구할 때 사용됩니다.)

  ex) 45 * 99 = 4455
  input: {:id 99, :total-sleep-time 30, :frequently-sleep-minute-and-times {:minute 45, :times 3}}
  output: 4455
  "
  [{:keys [frequently-sleep-minute-and-times id] :as _sleep-summary}]
  (* (:minute frequently-sleep-minute-and-times) id))

(defn part1-solution
  []
  (->> (read-file-as-list "resources/aoc2018_4.txt")
       (map parse-and-get-sleep-info)
       (sort-by :time)
       (reduce track-sleep-for-each-guard [])
       group-by-guard-and-get-sleep-summary
       get-most-total-sleep-time-between-guard
       multiply-minute-and-id))

(defn part2-solution
  []
  (->> (read-file-as-list "resources/aoc2018_4.txt")
       (map parse-and-get-sleep-info)
       (sort-by :time)
       (reduce track-sleep-for-each-guard [])
       group-by-guard-and-get-sleep-summary
       get-most-frequently-sleep-minuite-between-guard
       multiply-minute-and-id))

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
  (part2-solution)
  (into [] (frequencies [1 2 3 4 5 1]))
  (frequencies [1 2 3 4 5 1])
  (re-find #"(\d+):(\d+)" "[1518-11-05 00:03] Guard #99 begins shift")
  (re-find #"\[(.+)\]" "[1518-11-05 00:03] Guard #99 begins shift")
  (re-find #"\[.+\] (\w+)" "[1518-11-05 00:03] Guard #99 begins shift")
  (re-find #"#(\d+)" "[1518-11-05 00:03] Guard #99 begins shift")
  (re-find #"#(\d+)" "[1518-11-04 00:36] falls asleep")
  (sort-by :time
           [(parse-and-get-sleep-info "[1518-11-05 00:03] Guard #99 begins shift")
            (parse-and-get-sleep-info "[1518-11-04 00:36] falls asleep")
            (parse-and-get-sleep-info "[1518-11-05 00:55] wakes up")])
  (get-sleep-summary [99 [{:id 99 :sleep-info [1 10 25 35]}]])
  (group-by-guard-and-get-sleep-summary [{:id 10, :sleep-info [5 25 30 55]} {:id 99, :sleep-info [40 50]} {:id 10, :sleep-info [24 29]} {:id 99, :sleep-info [36 46]} {:id 99, :sleep-info [45 55]}])
  (sort-by val (frequencies [1 2 3 4 5 1])))
