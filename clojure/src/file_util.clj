(ns file-util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-file-as-list
  "파일을 라인별로 분리된 리스트로 리턴해주는 함수입니다."
  [file-name]
  (-> file-name
      io/resource
      slurp
      (string/split #"\n")))

(comment
  (io/resource  "aoc2018_1_1.txt")
  (read-file-as-list "aoc2018_1_1.txt"))
