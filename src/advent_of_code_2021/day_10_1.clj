(require '[clojure.string :as str])

(defn syntax-checker [program]
  (loop [stack (list (first program))
         remaining-program (rest program)]

    (if (empty? remaining-program)
      (if (empty? stack) :healthy :incomplete)
      (let [current-token (first remaining-program)
            top-stack-token (first stack)]

        (case current-token
          \) (if (= \( top-stack-token) (recur (rest stack) (rest remaining-program)) {:syntax-error 3})
          \] (if (= \[ top-stack-token) (recur (rest stack) (rest remaining-program)) {:syntax-error 57})
          \} (if (= \{ top-stack-token) (recur (rest stack) (rest remaining-program)) {:syntax-error 1197})
          \> (if (= \< top-stack-token) (recur (rest stack) (rest remaining-program)) {:syntax-error 25137})
          (recur (conj stack current-token) (rest remaining-program)))))))

(defn solve [input]
  (->>
   (str/split-lines input)
   (map seq)
   (map #(:syntax-error (syntax-checker %) 0))
   (apply +)))