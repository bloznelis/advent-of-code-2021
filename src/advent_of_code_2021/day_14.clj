(require '[clojure.string :as s])
(require '[advent-of-code-2021.utils :as util])

(defn create-new-pairs [[key val] rules]
  (hash-map (str (first key) (rules key)) val (str (rules key) (second key)) val))

(defn solve [input steps]
  (let [blocks (util/parse-blocks input)
        template (first blocks)
        ruless (->> (s/split-lines (second blocks))
                    (map #(s/split % #" -> "))
                    (into {}))
        initial-state (->> (partition 2 1 template)
                           (mapcat #(hash-map (s/join %) 1)) ;; can contain duplicates - so it should be more than 1 in that case
                           (into {}))]
    (loop [current-step 0
           state initial-state]

      (if (= current-step steps)
        (as-> (reduce  #(merge-with + %1 %2) (map #(hash-map (second (first %)) (second %)) state)) freqs
          (- (val (apply max-key second freqs)) (val (apply min-key second freqs))))
        (recur (inc current-step) (reduce #(merge-with + %1 %2) (map #(create-new-pairs % ruless) state)))))))

(defn p1 [input] (solve input 10))
(defn p2 [input] (solve input 40))
