(ns aoc2018_3
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(defn read-file-as-list "파일을 라인별로 분리된 리스트로 리턴해주는 함수입니다." [input-src]
  (clojure.string/split (slurp input-src) #"\n"))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(defn get-cartesian-product
  "
  입력값에 대해서 전체 좌표(cartesian-product)를 구하는 함수입니다.
  ex)
  input: {:id 1 :x 2 :y 2 :width 2 :height 2}
  output: {:id 1 :cart ((2 2) (2 3) (3 2) (3 3))}
  "
  [{id :id x :x y :y width :width height :height}]
  {:id id
   :cart (combo/cartesian-product (into [] (range x (+ x width))) (into [] (range y (+ y height))))})

(defn parse-and-get-claim-info
  "
  텍스트 라인(claim)을 읽어서 id, x, y, width, height 정보를 리턴해주는 함수입니다.
  ex)
  input: #1 @ 49,222: 19x20
  output: {:id 1 :x 49 :y 222 :width 19 :height 20}
  "
  [line]
  (let [id (Integer/parseInt (get (string/split (get (string/split line #"#") 1) #" @ ") 0))
        x (Integer/parseInt (get (string/split (get (string/split line #" @ ") 1) #",") 0))
        y (Integer/parseInt (get (string/split (get (string/split line #",") 1) #": ") 0))
        width (Integer/parseInt (get (string/split (get (string/split line #": ") 1) #"x") 0))
        height (Integer/parseInt (get (string/split line #"x") 1))]
    {:id id :x x :y y :width width :height height}))

(defn get-coordinate-id-set
  "
  현재까지 매칭된 좌표값(map-value)에 대해서 추가적으로 일치하는 좌표에 대해서
  ex)
  input: id: 1 map-value: {(2 3) #{2} (3 4) #{3}} coordinate: (2 3)
  output: {(2 3) #{2 1} (3 4) #{3}}
  "
  [{:keys [id cart]}]
  (->> cart
       (map (fn [coordinate] {:coordinate coordinate :id-set #{id}}))))

(defn merge-coordinate-id-set
  [coordinate-map {:keys [coordinate id-set]}]
  (let [found-id-set  (get coordinate-map coordinate)]
    (if found-id-set
      (assoc coordinate-map coordinate (set/union found-id-set id-set))
      (assoc coordinate-map coordinate id-set))))

(defn get-cart-list []
  (->> (read-file-as-list "resources/aoc2018_3.txt")
       (map parse-and-get-claim-info)
       (map get-cartesian-product)))

(defn get-id-list []
  (->> (read-file-as-list "resources/aoc2018_3.txt")
       (map parse-and-get-claim-info)
       (map #(get % :id))
       (into #{})))

(defn get-merged-coordinate-id-set
  []
  (->> (get-cart-list)
       (map get-coordinate-id-set)
       flatten
       (reduce merge-coordinate-id-set {})))

(defn part1-solution []
  (->> (get-merged-coordinate-id-set)
       vals
       (filter #(> (count %) 1))
       count))

(defn part2-solution []
  (->> (get-merged-coordinate-id-set)
       vals
       (filter #(> (count %) 1))
       (reduce set/union)
       (set/difference (get-id-list))))

(comment
  (into #{} [1 2 3])
  (into [] (range 1 4))
  (get-cartesian-product {:x 1 :y 3 :width 4 :height 4})
  (parse-and-get-claim-info "#100 @ 1,3: 4x4")
  (part1-solution)
  (part2-solution)
  (let [some-map {}]
    (assoc some-map :a 1)
    (assoc some-map :b 1))
  (combo/permutations [1 1 2])
  (set/intersection (into #{} [{:x 1 :y 2} {:x 1 :y 3}]) (into #{} [{:x 1 :y 2} {:x 2 :y 4}])))
