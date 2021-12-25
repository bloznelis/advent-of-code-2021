(require '[clojure.string :as str])

(defn inc-east [[x y]]
  (let [increased-x (inc x)]
    (if (< 138 increased-x)
      [0 y]
      [increased-x y])))

(defn inc-south [[x y]]
  (let [increased-y (inc y)]
    (if (< 136 increased-y)
      [x 0]
      [x increased-y])))

(defn occupied? [matrix coords]
  (case (matrix coords) "." false true))

(defn can-move-east [matrix [cell-coords cell-value]]
  (case cell-value
    ">" (not (occupied? matrix (inc-east cell-coords)))
    false))

(defn can-move-south [matrix [cell-coords cell-value]]
  (case cell-value
    "v" (not (occupied? matrix (inc-south cell-coords)))
    false))

(defn move [[cell-coords cell-value]]
  (case cell-value
    "v" {(inc-south cell-coords) cell-value cell-coords "."}
    ">" {(inc-east cell-coords) cell-value cell-coords "."}))

(defn move-east-herd [matrix]
  (let [movables (filter #(can-move-east matrix %) matrix)]
    (if (empty? movables)
      matrix
      (->>
       (map #(move %) movables)
       (apply conj)
       (conj matrix)))))

(defn move-south-herd [matrix]
  (let [movables (filter #(can-move-south matrix %) matrix)]
    (if (empty? movables)
      matrix
      (->>
       (map #(move %) movables)
       (apply conj)
       (conj matrix)))))

(defn move-agurks [matrix]
  (loop [current-matrix matrix
         step 1]
    (let [east-movables (filter #(can-move-east current-matrix %) current-matrix)
          south-movables (filter #(can-move-south current-matrix %) current-matrix)]
      (if (= (count (concat south-movables east-movables)) 0)
        step
        (recur (->> (move-east-herd current-matrix)
                    (move-south-herd)) (inc step))))))

(defn row-coords [row-idx row]
  (->> (str/split row #"")
       (map-indexed #(hash-map [%1 row-idx] %2))))

(defn read-map [input]
  (->> (str/split-lines input)
       (map-indexed #(row-coords %1 %2))
       (flatten)
       (apply conj)))

(defn solve [input]
  (move-agurks (read-map input)))