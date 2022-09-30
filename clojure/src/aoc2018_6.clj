(ns aoc2018_6
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [file-util :refer [read-file-as-list]]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)


;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coord A: abs(4-1) + abs(3-1) =  5
;; Distance to coord B: abs(4-1) + abs(3-6) =  6
;; Distance to coord C: abs(4-8) + abs(3-3) =  4
;; Distance to coord D: abs(4-3) + abs(3-4) =  2
;; Distance to coord E: abs(4-5) + abs(3-5) =  3
;; Distance to coord F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(def asc <)
(def desc >)

(defn parse-coord
  "
  input: 10, 12
  output: {:x 10 :y 12}
  "
  [coord-str]
  (->> coord-str
       (re-find #"(\d+), (\d+)")
       ((fn [[_ x y]] {:x (Integer/parseInt x) :y (Integer/parseInt y)}))))

(defn calc-manhattan-dist
  "
  절대거리를 구해주는 함수입니다. 1 + 3 = 4
  input: {:x 1 :y 1} {:x 2 :y 4}
  output: 4
  "
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (+ (abs (- x1 x2))
     (abs (- y1 y2)))) ; 1.10.x -> 1.11.x (abs )

(defn sort-by-manhattan-dist-asc
  [coords {:keys [x y]}]
  (->> coords
       (map (fn [coord] {:coord coord
                         :manhattan-dist (calc-manhattan-dist coord {:x x :y y})}))
       (sort-by :manhattan-dist asc)))

(defn get-unique-closest-coordiate-or-nil
  "
  오름차순으로 정렬된 좌표들을 바탕으로 거리가 제일 가까운 좌표를 구해주는 함수입니다.
  제일 가까운 좌표가 여러개일 때는 nil을 반환합니다.

  ex1) 거리가 3이 나오는 좌표가 2개일 때 -> nil
  input: ({:coord {:x 1 :y 1} :manhattan-dist 3} {:coord {:x 4 :y 3} :manhattan-dist 3} {:coord {:x 5 :y 41} :manhattan-dist 5})
  output: nil

  ex2) 거리가 3이 나오는 좌표가 1개일 때 -> 좌표값
  input: ({:coord {:x 1 :y 1} :manhattan-dist 3} {:coord {:x 4 :y 4} :manhattan-dist 4} {:coord {:x 5 :y 41} :manhattan-dist 5})
  output: {:x 1 :y 1}
  "
  [asc-sorted-coords]
  (when-not (= (:manhattan-dist (first asc-sorted-coords))
               (:manhattan-dist (second asc-sorted-coords)))
    (:coord (first asc-sorted-coords))))

(defn get-area-limits
  "
  주어진 좌표들에 대해서 최대 boundary를 리턴합니다. 최소/최대 x/y 좌표들을 구해주는 함수입니다.

  ex)
  input: ({:x 1 :y 1} {:x 2 :y 4} {:x 4 :y 4})
  output: {:min-x 1 :min-y 2 :max-x 4 :max-y 4}
  "
  [coords]
  {:min-x (apply min (map :x coords))
   :min-y (apply min (map :y coords))
   :max-x (apply max (map :x coords))
   :max-y (apply max (map :y coords))})

(defn get-coords-in-area-limits
  "
  주어진 area-limit 안에 있는 모든 좌표값들을 구하는 함수입니다.

  ex)
  input: {:min-x 1 :min-y 1 :max-x 4 :max-y 4}
  output: ({:x 1 :y 1} {:x 1 :y 2} ... {:x 4 :y 3} {:x 4 :y 4})
  "
  [{:keys [min-x min-y max-x max-y] :as _area_limits}]
  (for [y (range min-y (inc max-y))
        x (range min-x (inc max-x))]
    {:x x :y y}))

(defn get-closest-coord-in-area-limits
  "
  주어진 area-limit 안에 있는 좌표들에 대해서 각각 manhattan 거리가 제일 가까운 좌표값을 구하는 함수입니다.
  아래 그림에서 소문자/점 부분들을 실제로 구해주는 함수라고 생각해주시면 됩니다.

  aaaaa.cccc
  aAaaa.cccc
  aaaddecccc
  aadddeccCc
  ..dDdeeccc
  bb.deEeecc
  bBb.eeee..
  bbb.eeefff
  bbb.eeffff
  bbb.ffffFf

  ex)
  input: ({:x 1 :y 1} {:x 3 :y 2} {:x 4 :y 4}) {:min-x 1 :min-y 1 :max-x 4 :max-y 4}
  output: ({:x 1 :y 1} {:x 1 :y 1} nil {:x 3 :y 2} nil {:x 4 :y 4} ...)
  "
  [coords area-limits]
  (->> (get-coords-in-area-limits area-limits)
       (map #(sort-by-manhattan-dist-asc coords %))
       (map get-unique-closest-coordiate-or-nil)))

;; 모든 좌표들을 구할 필요 없이 가장자리에 있는 좌표들을 구할 수 있다.
(defn get-border-coords
  "
  주어진 area-limit의 가장자리에 있는 좌표들을 구해주는 함수입니다.
  가장자리에 있는 좌표의 경우 infinite으로 뻗어나가는 좌표라고 볼 수 있기에 가장자리까지 증식에 성공한 좌표들을 구해줍니다.

  ex)
  input: ({:x 1 :y 1} {:x 3 :y 2} {:x 3 :y 3} {:x 4 :y 4}) {:min-x 1 :min-y 1 :max-x 4 :max-y 4}
  output: {:top ({:x 1 :y 1} {:x 1 :y 1} nil nil)
           :bottom (nil nil nil {:x 4 :y 4})
           :left (...) :right (...)}
  "
  [width height closest-cord-in-area-limit]
  {:top (map #(nth closest-cord-in-area-limit %) (range 0 width))
   :bottom (map #(nth closest-cord-in-area-limit %) (range (* (dec height) width) (* height width)))
   :left (map #(nth closest-cord-in-area-limit %) (range 0 (* height width) width))
   :right (map #(nth closest-cord-in-area-limit %) (range (dec width) (* height width) width))})

(defn get-infinite-coords
  "
  get-border-coords 함수에서 구한 가장자리 좌표들을 바탕으로
  무한히 뻗어나갈 수 있는 좌표들의 set을 구해주는 함수입니다.

  ex)
  input: ({:x 1 :y 1} {:x 3 :y 2} {:x 3 :y 3} {:x 4 :y 4}) {:min-x 1 :min-y 1 :max-x 4 :max-y 4}
  output: #{{:x 1 :y 1} {:x 4 :y 4}}
  "
  [closest-cord-in-area-limit {:keys [min-x min-y max-x max-y] :as _area_limits}]
  (let [width (inc (- max-x min-x))
        height (inc (- max-y min-y))
        {:keys [bottom top left right]} (get-border-coords width height closest-cord-in-area-limit)]
    (into #{} (filter some? (concat bottom top left right))))) ;; filter some? into keep -> 리팩토링 필요

(defn get-infinite-coords-v2
  "
  get-border-coords 함수에서 구한 가장자리 좌표들을 바탕으로
  무한히 뻗어나갈 수 있는 좌표들의 set을 구해주는 함수입니다.

  ex)
  input: ({:x 1 :y 1} {:x 3 :y 2} {:x 3 :y 3} {:x 4 :y 4}) {:min-x 1 :min-y 1 :max-x 4 :max-y 4}
  output: #{{:x 1 :y 1} {:x 4 :y 4}}
  "
  [coords {:keys [min-x min-y max-x max-y] :as _area_limits}]
  (->> coords
       (keep (fn [{:keys [x y]}] (or (= x min-x) (= x max-x) (= y min-y) (= y max-y))))
       (into #{})))

(defn get-area-summary
  "
  입력 좌표값들에 대해서 요약을 해주는 함수입니다.

  ex)
  input: ({:x 1 :y 1} {:x 3 :y 2} {:x 3 :y 3} {:x 4 :y 4})
  output: {:area-limits {:min-x 1 :min-y 2 :max-x 4 :max-y 4}
           :closest-coord-in-area-limits ({:x 1 :y 1} {:x 1 :y 1} nil {:x 3 :y 2} nil {:x 4 :y 4} ...)
           :infinite-coords #{{:x 1 :y 1} {:x 4 :y 4}}
           :finite-coords #{{:x 3 :y 2} {:x 3 :y 3}}}
  "
  [coords]
  (let [area-limits (get-area-limits coords)
        closest-coord-in-area-limits (get-closest-coord-in-area-limits coords area-limits)
        infinite-coords (get-infinite-coords-v2 coords area-limits)
        finite-coords (set/difference (into #{} coords) infinite-coords)]
    {:area-limits area-limits
     :closest-coord-in-area-limits closest-coord-in-area-limits
     :infinite-coords infinite-coords
     :finite-coords finite-coords}))

(defn filter-finite-coord
  "
  area-summary를 바탕으로 무한히 뻗어나가지 않는 좌표들만 가져오는 함수입니다.
  실제 matrix상의 좌표값들을 가져오기에 겹치는 값들이 있을 수 있습니다.

  ex)
  input: {:area-limits {:min-x 1 :min-y 2 :max-x 4 :max-y 4}
           :closest-coord-in-area-limits ({:x 1 :y 1} {:x 1 :y 1} nil {:x 3 :y 2} nil {:x 4 :y 4} ...)
           :infinite-coords #{{:x 1 :y 1} {:x 4 :y 4}}
           :finite-coords #{{:x 3 :y 2} {:x 3 :y 3}}}
  output: ({:x 3 :y 2} {:x 3 :y 2} {:x 3 :y 3} {:x 3 :y 3})
  "
  [{:keys [closest-coord-in-area-limits finite-coords] :as _area-summary}]
  (filter #(finite-coords %) closest-coord-in-area-limits))
;; contains? 위험성? vector같은 곳에서는 쓰지 않는게 좋다.
;; filter-finite-coord 함수의 경우 사실상 함수를 읽어보면 읽는 에너지가 똑같다. 별도 함수로 안만들어도 될 것
;; some
;; (:a {:a 1}) -> 1
;; set도 함수다 truthy fasly
(defn get-larget-area
  "
  filter-finite-coord 함수를 바탕으로 유한한 영역 중 제일 큰 영역의 크기를 구합니다.

  ex)
  input: ({:x 3 :y 2} {:x 3 :y 2} {:x 3 :y 3} {:x 3 :y 3})
  output: 2
  "
  [finite-coords]
  (->> finite-coords
       frequencies
       (apply max-key val)
       val))

(defn part1-solution
  []
  (->> (read-file-as-list "aoc2018_6.txt")
       (map parse-coord)
       get-area-summary
       filter-finite-coord
       get-larget-area))

(defn calc-manhattan-dist-in-area-limits
  "
  area-limits 안에 있는 모든 좌표들에 대해 각 좌표별로 manhattan 거리를 구한 값들의 합을 구합니다.

  ex)
  input: ({:x 1 :y 1} ... {:x 4 :y 4}) ({:x 1 :y 1} ... {:x 4 :y 4})
  output: (8 7 9 3 17 18 ...)
  "
  [coords coord]
  (->> coords
       (map #(calc-manhattan-dist coord %))
       (reduce +)))

;; (clojure.java.io/resource "aoc2018_6.txt")
;; border에 대해서 숫자가 이미 10000이 넘어간다. 따라서 border 내에서 10000인 좌표들을 찾으면 된다.
(defn part2-solution
  [] ; Coord
  (let [coords (->> (map parse-coord (read-file-as-list "aoc2018_6.txt")))
        area-summary (get-area-summary coords)]
    (->> coords
         get-area-summary
         (#(get-coords-in-area-limits (:area-limits %)))
         (map #(calc-manhattan-dist-in-area-limits coords %))
         (filter #(< % 32))
         count)))


(comment
  (parse-coord "1, 2")
  (apply min [1 2 3])
  (calc-manhattan-dist {:x 1 :y 2} {:x 2 :y 3})
  (contains? #{{:x 1 :y 2} {:x 2 :y 3}} {:x 2 :y 4})
  (part1-solution)
  (part2-solution)
  (->> (read-file-as-list "resources/aoc2018_6.txt")
       (map parse-coord)
       get-area-summary))

;; TODO
;; day6 Refactoring
;; read-file-as-list -> util으로 빼서 require 해서 사용하도록 분리해보자.
;; Character 기반으로 reduce 돌려보기(?) -> String 비교를 Character로 진행해보기
;; 추가 과제: thread first와 thread last는 언제 어떻게 쓰는게 좋을까? 혹은... 왜 어떤 구현은 thread first이고 thread last일까?
;; (map f coll) thread last
;; (string str ...) thread first
;; 2020년 1번문제부터 다시 쭉 풀기 (GOAL : 1 -> 4 -> 6 풀어보기 til tomorrow)
;; 4번 풀 때는 clojure spec 보고 풀어보면 좋겠다.
;; https://clojure.org/guides/spec
;; https://johngrib.github.io/wiki/clojure/guide/spec/