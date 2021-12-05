(require '[clojure.string :as str])

(defn all-lines [[x1 y1 x2 y2]]
  (let [x-step (compare x2 x1)
        y-step (compare y2 y1)]

    (loop [lines-acc [[x1 y1]]]
      (let [[last-x last-y] (last lines-acc)
            new-coord [(+ last-x x-step) (+ last-y y-step)]]

        (if (= new-coord [x2 y2])
          (conj lines-acc new-coord)
          (recur (conj lines-acc new-coord)))))))

(defn solve [input]
  (->>
   (str/split-lines input)
   (map #(str/split % #" -> |,"))
   (map #(mapv read-string %))
   (mapcat all-lines)
   (frequencies)
   (vals)
   (filter #(> % 1))
   (count)))