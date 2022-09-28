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

(def asc <)
(def desc >)

(defn parse-coordinate
  "
  input: 10, 12
  output: {:x 10 :y 12}
  "
  [coordinate-str]
  (->> coordinate-str
       (re-find #"(\d+), (\d+)")
       ((fn [[_ x y]] {:x (Integer/parseInt x) :y (Integer/parseInt y)}))))

(defn calculate-manhattan-dist
  "
  절대거리를 구해주는 함수입니다. 1 + 3 = 4
  input: {:x 1 :y 1} {:x 2 :y 4}
  output: 4
  "
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn sort-by-manhattan-dist-asc
  [coordinates {:keys [x y]}]
  (->> coordinates
       (map (fn [coordinate] {:coordinate coordinate
                              :manhattan-dist (calculate-manhattan-dist coordinate {:x x :y y})}))
       (sort-by :manhattan-dist asc)))

(defn get-unique-closest-coordiate-or-nil
  "
  오름차순으로 정렬된 좌표들을 바탕으로 거리가 제일 가까운 좌표를 구해주는 함수입니다.
  제일 가까운 좌표가 여러개일 때는 nil을 반환합니다.

  ex1) 거리가 3이 나오는 좌표가 2개일 때 -> nil
  input: ({:coordinate {:x 1 :y 1} :manhattan-dist 3} {:coordinate {:x 4 :y 3} :manhattan-dist 3} {:coordinate {:x 5 :y 41} :manhattan-dist 5})
  output: nil

  ex2) 거리가 3이 나오는 좌표가 1개일 때 -> 좌표값
  input: ({:coordinate {:x 1 :y 1} :manhattan-dist 3} {:coordinate {:x 4 :y 4} :manhattan-dist 4} {:coordinate {:x 5 :y 41} :manhattan-dist 5})
  output: {:x 1 :y 1}
  "
  [asc-sorted-coordinates]
  (when-not (= (:manhattan-dist (first asc-sorted-coordinates))
               (:manhattan-dist (second asc-sorted-coordinates)))
    (:coordinate (first asc-sorted-coordinates))))

(defn get-area-limits
  "
  주어진 좌표들에 대해서 최대 boundary를 리턴합니다. 최소/최대 x/y 좌표들을 구해주는 함수입니다.

  ex)
  input: ({:x 1 :y 1} {:x 2 :y 4} {:x 4 :y 4})
  output: {:min-x 1 :min-y 2 :max-x 4 :max-y 4}
  "
  [coordinates]
  {:min-x (apply min (map :x coordinates))
   :min-y (apply min (map :y coordinates))
   :max-x (apply max (map :x coordinates))
   :max-y (apply max (map :y coordinates))})

(defn get-coordinates-in-area-limits
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

(defn get-closest-coordinate-in-area-limits
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
  [coordinates area-limits]
  (->> (get-coordinates-in-area-limits area-limits)
       (map #(sort-by-manhattan-dist-asc coordinates %))
       (map get-unique-closest-coordiate-or-nil)))

(defn get-border-coordinates
  "
  주어진 area-limit의 가장자리에 있는 좌표들을 구해주는 함수입니다.
  가장자리에 있는 좌표의 경우 infinite으로 뻗어나가는 좌표라고 볼 수 있기에 가장자리까지 증식에 성공한 좌표들을 구해줍니다.

  ex)
  input: ({:x 1 :y 1} {:x 3 :y 2} {:x 3 :y 3} {:x 4 :y 4}) {:min-x 1 :min-y 1 :max-x 4 :max-y 4}
  output: {:top ({:x 1 :y 1} {:x 1 :y 1} nil nil)
           :bottom (nil nil nil {:x 4 :y 4})
           :left (...) :right (...)}
  "
  [width height closest-coorrdinate-in-area-limit]
  {:top (map #(nth closest-coorrdinate-in-area-limit %) (range 0 width))
   :bottom (map #(nth closest-coorrdinate-in-area-limit %) (range (* (dec height) width) (* height width)))
   :left (map #(nth closest-coorrdinate-in-area-limit %) (range 0 (* height width) width))
   :right (map #(nth closest-coorrdinate-in-area-limit %) (range (dec width) (* height width) width))})

(defn get-infinite-coordinates
  "
  get-border-coordinates 함수에서 구한 가장자리 좌표들을 바탕으로
  무한히 뻗어나갈 수 있는 좌표들의 set을 구해주는 함수입니다.

  ex)
  input: ({:x 1 :y 1} {:x 3 :y 2} {:x 3 :y 3} {:x 4 :y 4}) {:min-x 1 :min-y 1 :max-x 4 :max-y 4}
  output: #{{:x 1 :y 1} {:x 4 :y 4}}
  "
  [closest-coorrdinate-in-area-limit {:keys [min-x min-y max-x max-y] :as _area_limits}]
  (let [width (inc (- max-x min-x))
        height (inc (- max-y min-y))
        {:keys [bottom top left right]} (get-border-coordinates width height closest-coorrdinate-in-area-limit)]
    (into #{} (distinct (filter some? (concat bottom top left right))))))

(defn get-area-summary
  "
  입력 좌표값들에 대해서 요약을 해주는 함수입니다.

  ex)
  input: ({:x 1 :y 1} {:x 3 :y 2} {:x 3 :y 3} {:x 4 :y 4})
  output: {:area-limits {:min-x 1 :min-y 2 :max-x 4 :max-y 4}
           :closest-coordinate-in-area-limits ({:x 1 :y 1} {:x 1 :y 1} nil {:x 3 :y 2} nil {:x 4 :y 4} ...)
           :infinite-coordinates #{{:x 1 :y 1} {:x 4 :y 4}}
           :finite-coordinates #{{:x 3 :y 2} {:x 3 :y 3}}}
  "
  [coordinates]
  (let [area-limits (get-area-limits coordinates)
        closest-coordinate-in-area-limits (get-closest-coordinate-in-area-limits coordinates area-limits)
        infinite-coordinates (get-infinite-coordinates closest-coordinate-in-area-limits area-limits)
        finite-coordinates (set/difference (into #{} coordinates) infinite-coordinates)]
    {:area-limits area-limits
     :closest-coordinate-in-area-limits closest-coordinate-in-area-limits
     :infinite-coordinates infinite-coordinates
     :finite-coordinates finite-coordinates}))

(defn filter-finite-coordinate
  "
  area-summary를 바탕으로 무한히 뻗어나가지 않는 좌표들만 가져오는 함수입니다.
  실제 matrix상의 좌표값들을 가져오기에 겹치는 값들이 있을 수 있습니다.

  ex)
  input: {:area-limits {:min-x 1 :min-y 2 :max-x 4 :max-y 4}
           :closest-coordinate-in-area-limits ({:x 1 :y 1} {:x 1 :y 1} nil {:x 3 :y 2} nil {:x 4 :y 4} ...)
           :infinite-coordinates #{{:x 1 :y 1} {:x 4 :y 4}}
           :finite-coordinates #{{:x 3 :y 2} {:x 3 :y 3}}}
  output: ({:x 3 :y 2} {:x 3 :y 2} {:x 3 :y 3} {:x 3 :y 3})
  "
  [{:keys [closest-coordinate-in-area-limits finite-coordinates] :as _area-summary}]
  (filter #(contains? finite-coordinates %) closest-coordinate-in-area-limits))

(defn get-larget-area
  "
  filter-finite-coordinate 함수를 바탕으로 유한한 영역 중 제일 큰 영역의 크기를 구합니다.

  ex)
  input: ({:x 3 :y 2} {:x 3 :y 2} {:x 3 :y 3} {:x 3 :y 3})
  output: 2
  "
  [finite-coordinates]
  (->> finite-coordinates
       frequencies
       (apply max-key val)
       val))

(defn part1-solution
  []
  (->> (read-file-as-list "resources/aoc2018_6.txt")
       (map parse-coordinate)
       get-area-summary
       filter-finite-coordinate
       get-larget-area))

(defn calculate-manhattan-dist-in-area-limits
  "
  area-limits 안에 있는 모든 좌표들에 대해 각 좌표별로 manhattan 거리를 구한 값들의 합을 구합니다.

  ex)
  input: ({:x 1 :y 1} ... {:x 4 :y 4}) ({:x 1 :y 1} ... {:x 4 :y 4})
  output: (8 7 9 3 17 18 ...)
  "
  [coordinates coordinates-in-area-limits]
  (map (fn [coordinate-in-area-limit]
         (->> coordinates
              (map #(calculate-manhattan-dist coordinate-in-area-limit %))
              (reduce +))) coordinates-in-area-limits))

(defn smaller-than
  "
  limit보다 작은지를 비교해주는 함수를 리턴합니다.

  ex)
  input: 32
  output: (fn [x] (< x 32))

  usage)
  (filter (smaller-than 32) (30 50 20)) -> (30 20)
  "
  [limit]
  (fn [x] (< x limit)))

;; border에 대해서 숫자가 이미 10000이 넘어간다. 따라서 border 내에서 10000인 좌표들을 찾으면 된다.
(defn part2-solution
  []
  (let [coordinates (->> (map parse-coordinate (read-file-as-list "resources/aoc2018_6.txt")))]
    (->> coordinates
         get-area-summary
         (#(get-coordinates-in-area-limits (:area-limits %)))
         (calculate-manhattan-dist-in-area-limits coordinates)
         (filter (smaller-than 32))
         count)))


(comment
  (parse-coordinate "1, 2")
  (apply min [1 2 3])
  (calculate-manhattan-dist {:x 1 :y 2} {:x 2 :y 3})
  (contains? #{{:x 1 :y 2} {:x 2 :y 3}} {:x 2 :y 4})
  (part1-solution)
  (part2-solution)
  (->> (read-file-as-list "resources/aoc2018_6.txt")
       (map parse-coordinate)
       get-area-summary))
