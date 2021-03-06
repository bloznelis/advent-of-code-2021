(require '[clojure.string :as str])

(defn handle-instruction [pos instruction]
  (let [instrucion-name (first instruction)
        value (second instruction)]

    (case instrucion-name
      "forward" (update pos :horizontal #(+ % (second instruction)))
      "down" (update pos :depth #(+ % value))
      "up" (update pos :depth #(- % value)))))

(defn run [pos instructions]
  (if (empty? instructions)
    (* (:horizontal pos) (:depth pos))
    (recur (handle-instruction pos (first instructions)) (rest instructions))))

(defn solve [input]
  (->>
   (str/split-lines input)
   (map #(str/split % #" "))
   (map #(list (first %) (Integer/parseInt (second %))))
   (run {:horizontal 0, :depth 0})))