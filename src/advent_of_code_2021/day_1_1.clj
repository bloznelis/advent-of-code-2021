(ns advent-of-code-2021.day_1_1.clj
  (:require [advent-of-code-2021.utils :as util]))

(defn solve [input]
  (->>
   (partition 2 1 (util/parse-ints input))
   (map #(apply < %))
   (filter true?)
   (count)))
