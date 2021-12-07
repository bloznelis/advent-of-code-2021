(ns advent-of-code-2021.utils
  (:require [clojure.string :as str]))

(defn to-decimal [binary-seq]
  (->>  (reverse binary-seq)
        (map-indexed #(* %2 (Math/pow 2 %1)))
        (apply +)))

(defn transpose [m]
  (apply mapv vector m))

(defn parse-ints [raw-input]
  (->>
   (str/split-lines raw-input)
   (map #(Integer/parseInt %))))

(defn tokenize [raw-input]
  (->>
   (str/split-lines raw-input)
   (mapv #(mapv read-string (str/split % #"")))))

(defn parse-blocks [raw-input]
  (str/split raw-input #"\n\n"))

(defn median [points]
  (let [sorted-points (sort points)
        size (count sorted-points)
        mid-point (/ size 2)]
    (if (odd? size)
      (nth sorted-points mid-point)
      (/ (+ (nth sorted-points mid-point) (nth sorted-points (dec mid-point))) 2))))