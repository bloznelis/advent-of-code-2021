(require '[clojure.string :as str])

(defn sum-factorial [x]
  (if (= x 0)
    0
    (loop [n x f 1]
      (if (= n 1)
        f
        (recur (dec n) (+ f n))))))

(defn calc-fuel [to-position all-positions]
  (->>
   (map #(sum-factorial (Math/abs (- to-position %))) all-positions)
   (apply +)))

(defn go [all-positions]
  (let [max-position (apply max all-positions)]

    (loop [current-position 0
           lowest-fuel Integer/MAX_VALUE]

      (if (> current-position max-position)
        lowest-fuel
        (let [total-fuel-to-current-position (calc-fuel current-position all-positions)]
          (recur
           (inc current-position)
           (if (< total-fuel-to-current-position lowest-fuel)
             total-fuel-to-current-position
             lowest-fuel)))))))

(defn solve [input]
  (->>
   (str/split input #",")
   (mapv #(read-string %))
   (go)))