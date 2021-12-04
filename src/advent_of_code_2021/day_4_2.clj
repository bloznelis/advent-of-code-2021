(require '[clojure.string :as str])
(require '[advent-of-code-2021.utils :as util])

(defn remove-number [row number]
  (remove #(= number %) row))

(defn cover-number [board number]
  {:rows (map #(remove-number % number) (:rows board))
   :columns (map #(remove-number % number) (:columns board))})

(defn calc-score [board num]
  (* num (->>
          (:rows board)
          (flatten)
          (map #(read-string %))
          (apply +))))

;; this is probably really bad
(defn subtract [x y]
  (vec (filter #(not (contains? (set y) %)) x)))

(defn is-winner [board]
  (or (some? (some empty? (:rows board))) (some? (some empty? (:columns board)))))

(defn run [state]
  (loop [current-state state]
    (let [current-number (first (:numbers current-state))
          covered-boards (map #(cover-number % current-number) (:boards current-state))
          winner-boards (filter #(is-winner %) covered-boards)
          new-boards (subtract covered-boards winner-boards)
          ]
      
      (if (empty? new-boards)
        (calc-score (first winner-boards) (read-string current-number))
        (recur (->
                (update current-state :numbers #(rest %))
                (assoc :boards new-boards)))))))


(defn parse-board [board-string]
  (let [rows (->>
              (str/split board-string #"\n")
              (map #(str/split (str/trim %) #"\s+")))
        columns (util/transpose rows)]

    {:rows rows, :columns columns}))

(defn parse-state [blocks]
  {:numbers (str/split (first blocks) #","), :boards (map #(parse-board %) (rest blocks))})

(defn solve [input]
  (->>
   (util/parse-blocks input)
   (parse-state)
   (run)))