(require '[clojure.string :as str])

(defn handle-instruction [pos instruction]
  (let [instruction-name (first instruction)
        value (second instruction)]

    (case instruction-name
      "forward" (->
                 (update pos :horizontal #(+ % value))
                 (update :depth #(+ % (* (:aim pos) value))))
      "down" (update pos :aim #(+ % value))
      "up" (update pos :aim #(- % value)))))

(defn run [pos instructions]
  (if (empty? instructions)
    (* (:horizontal pos) (:depth pos))
    (recur (handle-instruction pos (first instructions)) (rest instructions))))

(defn solve [input]
  (->>
   (str/split-lines input)
   (map #(str/split % #" "))
   (map #(list (first %) (Integer/parseInt (second %))))
   (run {:horizontal 0, :depth 0, :aim 0})))