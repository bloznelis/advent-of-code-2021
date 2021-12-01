(ns advent-of-code-2021.day_1_2.clj
  (:require [advent-of-code-2021.utils :as util]))

(defn solve [input]
  (->>
   (partition 3 1 (util/parse-ints input))
   (map #(apply + %))
   (partition 2 1)
   (map #(apply < %))
   (filter true?)
   (count)))