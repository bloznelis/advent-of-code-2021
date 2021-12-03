(require '[advent-of-code-2021.utils :as util])

(defn matches-criteria [num idx criteria]
  (= (nth num idx) criteria))

(defn find-rating [nums make-criteria-fn]
  (loop [filtered-nums nums
         criteria-cursor 0]
    (if (= 1 (count filtered-nums))
      (first filtered-nums)
      (let [new-criteria (make-criteria-fn filtered-nums criteria-cursor)]
        (recur
         (filter #(matches-criteria % criteria-cursor new-criteria) filtered-nums)
         (inc criteria-cursor))))))

(defn make-criteria [nums idx sort-fn apply-fn]
  (let [full-criteria (->>
                       (util/transpose nums)
                       (map #(frequencies %))
                       (map #(into (sorted-map-by sort-fn) %))
                       (map #(key (apply apply-fn val %))))]

    (nth full-criteria idx)))

(defn make-oxygen-rating-criteria [nums idx]
  (make-criteria nums idx < max-key))

(defn make-co2-rating-criteria [nums idx]
  (make-criteria nums idx > min-key))

(defn find-oxygen-rating [nums]
  (util/to-decimal (find-rating nums make-oxygen-rating-criteria)))

(defn find-co2-rating [nums]
  (util/to-decimal (find-rating nums make-co2-rating-criteria)))

(defn life-support-rating [nums]
  (* (find-oxygen-rating nums) (find-co2-rating nums)))

(defn solve [input]
  (life-support-rating (util/tokenize input)))