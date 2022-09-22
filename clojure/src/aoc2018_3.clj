(ns aoc2018_3
  (:require [clojure.set :as set
             clojure.string :as string
             clojure.math.combinatorics :as combo]))

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

;; (defn cartesian-product-intersection [[{id1 :id cart1 :cart} {id2 :id cart2 :cart}]]
;;   {:ids [id1 id2] :cart-intersection (set/intersection (into #{} cart1) (into #{} cart2))})

;; 동작은 하지만 큰 수에 대해서 동작하지 않는 로직
;; (defn part1-solution []
;;   (->> (read-file-as-list "resources/aoc2018_3.txt")
;;        (map parse-and-get-coordinate)
;;        (map get-cartesian-product)
;;        (#(combo/combinations % 2))
;;        (map cartesian-product-intersection)
;;        (map #(get % :cart-intersection))
;;        (reduce set/union)
;;        (count)))


(defn append-id-to-map-on-matching-coordinate
  "
  현재까지 매칭된 좌표값(map-value)에 대해서 추가적으로 일치하는 좌표에 대해서
  ex)
  input: id: 1 map-value: {(2 3) #{2} (3 4) #{3}} coordinate: (2 3)
  output: {(2 3) #{2 1} (3 4) #{3}}
  "
  [id map-value coordinate]
  (let [prev-ids (get map-value coordinate)]
    (if prev-ids
      (assoc map-value coordinate (conj prev-ids id))
      (assoc map-value coordinate #{id}))))

(defn get-id-list-map-on-matching-coordinate 
  "
  현재까지 매칭된 좌표값(map-value)에 대해서 추가적으로 일치하는 좌표에 대해서
  ex)
  input: id: 1 map-value: {(2 3) #{2} (3 4) #{3}} coordinate: (2 3)
  output: {(2 3) #{2 1} (3 4) #{3}}
  "
  [cart-list]
  (loop [{id :id current-cart :cart} (first cart-list)
         rest-cart-list (rest cart-list)
         coordinate-map {}]
    (let [new-coordinate-map (reduce #(append-id-to-map-on-matching-coordinate id %1 %2) coordinate-map current-cart)]
      (if (empty? rest-cart-list)
        new-coordinate-map
        (recur (first rest-cart-list) (rest rest-cart-list) new-coordinate-map)))))

(defn part1-solution []
  (let [cart-list (->> (read-file-as-list "resources/aoc2018_3.txt")
                       (map parse-and-get-claim-info)
                       (map get-cartesian-product))]
    (->> cart-list
         (get-id-list-map-on-matching-coordinate)
         (vals)
         (filter #(> (count %) 1))
         (count))))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

;; 동작은 하지만 큰 수에 대해서 동작하지 않는 로직
;; (defn part2-solution []
;;   (->> (read-file-as-list "resources/aoc2018_3.txt")
;;        (map parse-and-get-coordinate)
;;        (map get-cartesian-product)
;;        (#(combo/combinations % 2))
;;        (map cartesian-product-intersection)
;;        (filter #(empty? (get % :cart-intersection)))
;;        (map #(get % :ids))
;;        (map #(into #{} %))
;;        (reduce set/intersection)
;;        ))

(defn part2-solution []
  (let [cart-list (->> (read-file-as-list "resources/aoc2018_3.txt")
                       (map parse-and-get-claim-info)
                       (map get-cartesian-product))
        id-set (->> (read-file-as-list "resources/aoc2018_3.txt")
                    (map parse-and-get-claim-info)
                    (map #(get % :id))
                    (into #{}))]
    (->> cart-list
         (get-id-list-map-on-matching-coordinate)
         (vals)
         (filter #(> (count %) 1))
         (reduce set/union)
         (set/difference id-set))))

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
  (append-id-to-map-on-matching-coordinate 1 {'(2 3) #{3}} '(2 3))
  (set/intersection (into #{} [{:x 1 :y 2} {:x 1 :y 3}]) (into #{} [{:x 1 :y 2} {:x 2 :y 4}])))
  