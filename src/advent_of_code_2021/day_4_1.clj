(require '[clojure.string :as str])
(require '[advent-of-code-2021.utils :as util])

;; (filter-row [1 2 3] 2)

;; (cover-number {:rows [[1] [1 2] [3] [4] [6]], :columns [[1] [1 2] [3] [4] [6]]} 2)

;; (winner-board [
;;                {:rows [[1] [1 2] [3] [4] [6]], :columns [[1] [1 2] [3] [4] [6]]}
;;                {:rows [[1] [1 2] [3] [4] []], :columns [[1] [1 2] [3] [4] [5]]}
;; ])

;; (filter not-empty [[1 2 3] [1] []])
;; (map #(filter empty? %) [[1 2 3] [1]])

;; (calc-score {:rows [[1] [1 2] [3] [4] [6]], :columns [[1] [1 2] [3] [4] [6]]})

;; (def stateroni (parse-state (->
;;                               (slurp "/home/lukas/code/clojure/advent-of-code-2021/input.txt")
;;                               (str/split #"\n\n"))))

;; stateroni
;; (update stateroni :numbers #(rest %))


(defn winner-board [boards]
  (let [filtered-boards (map #(hash-map :rows (filter not-empty (:rows %)) :columns (filter not-empty (:columns %))) boards)
        winner-board (filter (or #(< (count (:rows %)) 5) #(< (count (:columns %)) 5)) filtered-boards)]

    (cond
      (empty? winner-board) :no-winner
      :else (first winner-board))))

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

(defn run-games [state]
  (loop [current-state state]
    (let [current-number (first (:numbers current-state))
          boards (:boards current-state)
          filtered-boards (map #(cover-number % current-number) boards)
          maybe-winner-board (winner-board filtered-boards)
          ]

      (case maybe-winner-board
        :no-winner (recur (->
                           (update current-state :numbers #(rest %))
                           (assoc :boards filtered-boards)))
        (calc-score maybe-winner-board (read-string current-number))))))

(defn parse-board [board-string]
  (let [rows (->>
              (str/split board-string #"\n")
              (map #(str/split (str/trim %) #"\s+")))
        columns (util/transpose rows)]
        
    {:rows rows, :columns columns}))
    
(defn parse-state [blocks]
  {:numbers (str/split (first blocks) #","), :boards (map #(parse-board %) (rest blocks))})


(run-games (parse-state (->
              (slurp "/home/lukas/code/clojure/advent-of-code-2021/input.txt")
              (str/split #"\n\n"))))