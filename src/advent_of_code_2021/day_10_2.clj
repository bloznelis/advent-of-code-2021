(require '[clojure.string :as str])
(require '[advent-of-code-2021.utils :as util])

(defn syntax-checker [program]
  (loop [stack (list (first program))
         remaining-program (rest program)]

    (if (empty? remaining-program)
      (if (empty? stack) :healthy {:incomplete stack})
      (let [current-program-token (first remaining-program)
            top-stack-token (first stack)]

        (case current-program-token
          \) (if (= \( top-stack-token) (recur (rest stack) (rest remaining-program)) :syntax-error)
          \] (if (= \[ top-stack-token) (recur (rest stack) (rest remaining-program)) :syntax-error)
          \} (if (= \{ top-stack-token) (recur (rest stack) (rest remaining-program)) :syntax-error)
          \> (if (= \< top-stack-token) (recur (rest stack) (rest remaining-program)) :syntax-error)
          (recur (conj stack current-program-token) (rest remaining-program)))))))

(defn accumulate-score [scores]
  (reduce #(+ (* 5 %1) %2) 0 scores))

(defn autocomplete-score [program]
  (let [incomplete-stack (:incomplete (syntax-checker program))]
    (if incomplete-stack
      (->>
       (map #(case %
               \( 1
               \[ 2
               \{ 3
               \< 4) incomplete-stack)
       accumulate-score))))

(defn solve [input]
  (->>
   (str/split-lines input)
   (map seq)
   (map autocomplete-score)
   (remove nil?)
   sort
   util/median))