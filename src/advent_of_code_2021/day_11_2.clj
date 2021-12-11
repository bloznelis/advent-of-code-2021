(require '[clojure.string :as str])
(require '[advent-of-code-2021.utils :as util])

(defn neighbours [[x y]]
  (for [x-modifier [-1 0 1]
        y-modifier [-1 0 1]
        :when (not (and (= x (+ x x-modifier)) (= y (+ y y-modifier))))]

    [(+ x x-modifier) (+ y y-modifier)]))

(defn map-function-on-map-vals [m f]
  (->> (map #(update % 1 f) m)
       (into {})))


(defn inc-nonzero [x]
  (if (zero? x)
    x
    (inc x)))

(defn inc-global-energy [matrix-hash]
  (map-function-on-map-vals matrix-hash inc))


(defn inc-at [locations matrix-hash]
  (->>
   (map #(fn [matrix] (util/update-existing matrix % inc-nonzero)) locations)
   (reduce #(%2 %1) matrix-hash)))

(defn is-settled [matrix-hash]
  (->>
   (filter #(> (second %) 9) matrix-hash)
   (empty?)))

(defn hash-matrix [idx line]
  (->>
   (str/split line #"")
   (map-indexed #(hash-map [%1 idx] (read-string %2)))
   (reduce conj)))

(defn resolve [matrix-hash]
  (let [matrix-ref (ref matrix-hash)]

    (loop [i 0]
      (defn update-matrix [key]
        (ref-set matrix-ref (->>
                             (update @matrix-ref key (constantly 0))
                             (inc-at (neighbours key)))))

      (dorun (map #(if (> (second %) 9) (dosync (update-matrix (first %)))) @matrix-ref))

      (if (is-settled (deref matrix-ref))
        (deref matrix-ref)
        (recur (inc i))))

    @matrix-ref))

(defn count-zeros [matrix-hash]
  (->>
   (map #(second %) matrix-hash)
   (filter #(= % 0))
   (count)))


(defn make-steps [matrix-hash]
  (loop [steps-made 0
         current-matrix matrix-hash
         total-flashes 0]

    (if (= (count-zeros current-matrix) 100)
      steps-made
      (recur (inc steps-made) (resolve (inc-global-energy current-matrix)) (+ total-flashes (count-zeros current-matrix))))))

(defn solve [input]
  (->> (str/split-lines input)
       (map-indexed hash-matrix)
       (reduce conj)
       (make-steps)))