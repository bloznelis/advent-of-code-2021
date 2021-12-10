(require '[clojure.string :as str])
(require '[clojure.set :as s])

(defn get-point [[x y] matrix]
  (nth (nth matrix y nil) x nil))

(defn neighbours [x y]
  (for [x-modifier [-1 0 1]
        y-modifier [-1 0 1]
        :when (or (= x-modifier 0) (= y-modifier 0))]

    [(+ x x-modifier) (+ y y-modifier)]))

(defn find-low-points [matrix]
  (let [y-length (count matrix)
        x-length (count (first matrix))]
    (for [x (range 0 x-length)
          y (range 0 y-length)
          :let [current-point (get-point [x y] matrix)
                neighbours (remove nil? (neighbours x y))
                same-height-points (remove #(> % current-point) (remove nil? (map #(get-point % matrix) neighbours)))]]

      (case (count same-height-points)
        1 [x y]
        nil))))

(defn find-new-expansions [[x y] already-collected matrix]
  (s/difference (->>
                 (neighbours x y)
                 (remove #(or (nil? (get-point % matrix)) (= (get-point % matrix) 9)))
                 (set)) already-collected))


(defn calc-basin-size [lowpoint-xy matrix]
  (let [basin-acc (ref #{lowpoint-xy})]

    (defn populate-basin [xy]
      (let [new-expansions (find-new-expansions xy (deref basin-acc) matrix)]

        (dosync (ref-set basin-acc (s/union @basin-acc new-expansions)))

        (if (empty? new-expansions)
          #{}
          (->>
           (map #(populate-basin %) new-expansions)
           (reduce s/union)))))

    (populate-basin lowpoint-xy)

    (count @basin-acc)))

(defn solve [input]
  (let [matrix (->>
                (str/split-lines input)
                (mapv #(mapv read-string (str/split % #""))))
        low-points (remove nil? (find-low-points matrix))
        basins  (mapv #(calc-basin-size % matrix) low-points)]

    (->>
     (sort basins)
     (take-last 3)
     (apply *))))