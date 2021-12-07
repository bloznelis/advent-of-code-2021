(require '[advent-of-code-2021.utils :as util])

(defn calc-distances [points]
  (let [middle-point (util/median points)]
    (map #(Math/abs (- % middle-point)) points)))

(defn solve [input]
  (->>
   (util/num-list input)
   (calc-distances)
   (apply +)))