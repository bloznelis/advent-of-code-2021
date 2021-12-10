(require '[clojure.string :as str])
(require '[advent-of-code-2021.utils :as util])

(defn get-m [x y matrix]
  (nth (nth matrix y nil) x nil))

(defn neighbours [x y matrix]
  (for [x-modifier [-1 0 1]
        y-modifier [-1 0 1]
        :when (or (= x-modifier 0) (= y-modifier 0))]

    (get-m (+ x x-modifier) (+ y y-modifier) matrix)))

(defn find-low-points [matrix]
  (let [y-length (count matrix)
        x-length (count (first matrix))]
    (for [x (range 0 x-length)
          y (range 0 y-length)
          :let [current-point (get-m x y matrix)
                neighbours (remove nil? (neighbours x y matrix))
                same-height-points (remove #(> % current-point) neighbours)]]

      (case (count same-height-points)
        1 current-point
        nil))))

(defn solve [input]
  (->>
   (str/split-lines input)
   (mapv #(mapv read-string (str/split % #"")))
   (find-low-points)
   (remove nil?)
   (map inc)
   (apply +)))