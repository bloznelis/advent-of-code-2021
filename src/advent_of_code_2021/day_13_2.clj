(require '[clojure.string :as str])
(require '[advent-of-code-2021.utils :as util])

(defn fold-y [coordinates fold-point]
  (let [lower (filter #(< fold-point (second %)) coordinates)
        upper (filter #(> fold-point (second %)) coordinates)
        folded (mapv #(vector (first %) (- (second %) (* 2 (- (second %) fold-point)))) lower)]

    (concat upper folded)))

(defn fold-x [coordinates fold-point]
  (let [lower (filter #(< fold-point (first %)) coordinates)
        upper (filter #(> fold-point (first %)) coordinates)
        folded (mapv #(vector (- (first %) (* 2 (- (first %) fold-point))) (second %)) lower)]

    (concat upper folded)))

(defn parse-fold [line]
  (let [trimmed (str/join (drop 11 line))
        axis-value (str/split trimmed #"=")]

    (case (first axis-value)
      "x" {:x (read-string (second axis-value))}
      "y" {:y (read-string (second axis-value))})))

(defn parse-folds [input]
  (->>
   (str/split-lines input)
   (map parse-fold)))

(defn solve [input] ;; Check stdout
  (let [blocks (util/parse-blocks input)
        coords (->> (first blocks)
                    (str/split-lines)
                    (map #(str/split % #","))
                    (mapv #(mapv read-string %)))
        folds (parse-folds (second blocks))]

    (loop [current-coords coords
           current-folds folds]

      (if (empty? current-folds)
        (let [final-coords (set current-coords)]
          (defn symbol [x' y']
            (if (contains? final-coords [x' y'])
              "#"
              "."))

          (for [y (range 0 6)
                x (range 0 40)]

            (if (= x 39)
              (println (symbol x y))
              (print (symbol x y)))))

        (case (first (keys (first current-folds)))
          :x (recur (->>
                     (fold-x current-coords (:x (first current-folds)))
                     (distinct)) (rest current-folds))
          :y (recur (->>
                     (fold-y current-coords (:y (first current-folds)))
                     (distinct)) (rest current-folds)))))))