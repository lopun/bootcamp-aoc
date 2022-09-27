(ns aoc2018_6
  (:require [clojure.string :as string]
            [clojure.set :as set]))

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

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(defn read-file-as-list "파일을 라인별로 분리된 리스트로 리턴해주는 함수입니다." [input-src]
  (clojure.string/split (slurp input-src) #"\n"))

(defn calculate-manhattan-dist
  " 절대거리 구해주는 함수입니다. 1 + 3 = 4
  input: [1 1] [2 4]
  output: 4
  "
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn get-unique-closest-coordiate-or-nil
  [asc-sorted-coordinates]
  (when-not (= (:manhattan-dist (first asc-sorted-coordinates))
               (:manhattan-dist (second asc-sorted-coordinates)))
    (:coordinate (first asc-sorted-coordinates))))

(defn sort-by-manhattan-dist-asc
  [coordinates {:keys [x y]}]
  (->> coordinates
       (map (fn [coordinate] {:coordinate coordinate
                              :manhattan-dist (calculate-manhattan-dist coordinate {:x x :y y})}))
       (sort-by :manhattan-dist <)))

(defn get-closest-coordinate
  [coordinates coordinate]
  (get-unique-closest-coordiate-or-nil
   (sort-by-manhattan-dist-asc coordinates coordinate)))


(defn part1-solution
  []
  ())

(comment
  (get-closest-coordinate [{:x 1 :y 2} {:x 1 :y 3}] {:x 5 :y 5})
  (calculate-manhattan-dist {:x 1 :y 2} {:x 2 :y 3}))