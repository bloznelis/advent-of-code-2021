(require '[clojure.string :as str])

(defn make-range [a1 a2]
  (if (> a1 a2)
    (range a2 (inc a1))
    (range a1 (inc a2))))

(defn straight-lines [[x1 y1 x2 y2]]
  (for [x (make-range x1 x2)
        y (make-range y1 y2)
        :when (or (= x1 x2) (= y1 y2))]

    [x y]))

(defn solve [input]
  (->>
   (str/split-lines input)
   (map #(str/split % #" -> |,"))
   (map #(mapv read-string %))
   (mapcat straight-lines)
   (frequencies)
   (vals)
   (filter #(> % 1))
   (count)))