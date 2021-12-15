(require '[clojure.string :as str])
(require '[advent-of-code-2021.utils :as util])

(defn fold-x [coordinates]
  (let [lower (filter #(< 655 (first %)) coordinates)
        upper (filter #(> 655 (first %)) coordinates)
        folded (mapv #(vector (- (first %) (* 2 (- (first %) 655))) (second %)) lower)]

    (concat upper folded)))

(defn solve [input]
  (->> (util/parse-blocks input)
       first
       str/split-lines
       (map #(str/split % #","))
       (mapv #(mapv read-string %))
       fold-x
       distinct
       count))