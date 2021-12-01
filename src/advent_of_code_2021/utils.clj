(ns advent-of-code-2021.utils
  (:require [clojure.string :as str]))

(defn parse-ints [raw-input]
  (->>
   (str/split-lines raw-input)
   (map #(Integer/parseInt %))))