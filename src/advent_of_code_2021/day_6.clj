(require '[clojure.string :as str])

(defn evolve [[key value]]
  (cond
    (= key 0) {6 value, 8 value}
    :else {(dec key) value}))

(defn sum-vals-if-exists [coll key val]
  (if (contains? coll key)
    (update coll key #(+ % val))
    (conj coll {key val})))

(defn join-baby [input]
  (->> (mapcat vec input)
       (reduce (fn [map [key value]] (sum-vals-if-exists map key value)) {})))

(defn vaziuojam [days initial-frequencies]
  (loop [frequencies initial-frequencies
         day 0]
    (if (= day days)
      frequencies
      (recur (join-baby (map evolve frequencies)) (inc day)))))

(defn solve [input days]
  (->>
   (str/split input #",")
   (mapv #(read-string %))
   (frequencies)
   (vaziuojam days)
   (vals)
   (apply +)))

(defn p1 [input] (solve input 80))
(defn p2 [input] (solve input 256))
