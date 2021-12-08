(require '[clojure.string :as str])

(defn do-counting [output-values]
  (map #(case (count %)
          2 1
          4 1
          3 1
          7 1
          0) output-values))

(defn solve [input]
  (->>
   (str/split-lines input)
   (mapcat #(-> (str/split % #" \| ")
                (second)
                (str/split #" ")
                (do-counting)))
   (apply +)))