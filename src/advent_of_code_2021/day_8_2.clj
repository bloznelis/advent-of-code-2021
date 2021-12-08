(require '[clojure.string :as str])
(require '[clojure.set :as s])

(defn merge-lists [maps]
  (apply merge-with into
         (for [m maps, [k v] m]
           {k [v]})))

(defn to-sets [output-values]
  (->>
   (map #(hash-map (count %) (set %)) output-values)
   (merge-lists)))

(defn first-val [key sets]
  (first (sets key)))

(defn diff-and-first [x y]
  (first (s/difference x y)))

(defn make-translation-table [first-line-part]
  (let [sets (to-sets (str/split first-line-part #" "))
        fives (sets 5)
        two (first-val 2 sets)
        four (first-val 4 sets)
        a (diff-and-first (first-val 3 sets) two)
        three-d (first (filter #(clojure.set/subset? two %) fives))
        b (diff-and-first four three-d)
        d (diff-and-first (disj four b) two)
        g (first (->
                  (s/difference three-d two)
                  (disj a d)))
        five-d (first (filter #(and (contains? % a) (contains? % b)) fives))
        f (first (disj five-d a b d g))
        c (first (disj two f))
        e (first (disj (first-val 7 sets) a b c d f g))]

    {a \a, b \b, c \c, d \d, e \e, f \f, g \g}))

(defn lookup-digit [sorted-digit]
  (case sorted-digit
    "abcefg" "0"
    "cf" "1"
    "acdeg" "2"
    "acdfg" "3"
    "bcdf" "4"
    "abdfg" "5"
    "abdefg" "6"
    "acf" "7"
    "abcdefg" "8"
    "abcdfg" "9"))

(defn translate [line]
  (let [split-line (str/split line #" \| ")
        numbers (str/split (second split-line) #" ")
        translation-table (make-translation-table (first split-line))]

    (->> number
         (map #(translation-table %))
         sort
         str/join
         lookup-digit
         (for [number numbers])
         str/join
         Integer/parseInt)))

(defn solve [input]
  (->>
   (str/split-lines input)
   (map translate)
   (apply +)))