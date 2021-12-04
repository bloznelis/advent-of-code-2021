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

(defn filter-non-empty-board [board]
  (-> (update board :rows #(filter not-empty %))
      (update :columns #(filter not-empty %))))

(filter-non-empty-board {:rows [[1] [1 2] [3] [4] [6]], :columns [[] [1 2] [3] [4] [6]]})

(defn winner-board-2 [boards]
  (filter  #(or (< (count (:rows %)) 5) (< (count (:columns %)) 5)) boards)
  )

(winner-board-2 [
                 {:rows [[1 1 1 1 1] [2 2 2 2 2 2] [3] [4] [1] ], :columns [[1][1 2] [3] [4] [6]]}
                 {:rows [[3] [1 2] [3] [4] [6]], :columns [[3] [1 2] [3] [4] [6]]}
                 ])


(map filter-non-empty-board [{:rows [[1 1 1 1 1] [2 2 2 2 2 2] [3] [4] []], :columns [[1] [1 2] [3] [4] [6]]}
                             {:rows [[3] [1 2] [3] [4] [6]], :columns [[3] [1 2] [3] [4] [6]]}])


;; (map filter-non-empty-board '({:rows '('(1) '(2)) :columns '('(1) '(2))}))

(defn winner-board [boards]
  (let [filtered-boards (map filter-non-empty-board boards)
        winner-board (filter #(or (< (count (:rows %)) 5) (< (count (:columns %)) 5)) filtered-boards)]

    (println winner-board)
    (cond
      (empty? winner-board) :no-winner
      :else (first winner-board))))

(defn is-winner [board]
  (or (some? (some empty? (:rows board))) (some? (some empty? (:columns board)))))

(is-winner {:rows [[1 1 1 1 1] [2 2 2 2 2 2] [3] [4] [1]], :columns [[1] [1 2] [3] [4] [6]]})

(defn remove-number [row number]
  (remove #(= number %) row))

(remove-number [1 2 3] nil)

(defn cover-number [board number]
  {:rows (map #(remove-number % number) (:rows board))
   :columns (map #(remove-number % number) (:columns board))
   :index (:index board)
   })

(defn calc-score [board num]
  (* num (->>
          (:rows board)
          (flatten)
          (map #(read-string %))
          (apply +))))

;; this is probably really bad
(defn subtract [x y]
  (vec (filter #(not (contains? (set y) %)) x)))

;; (disj! {1 2} {1 2 4})

(subtract [1 2 3] [1 3])

(defn run-games [state]
  (loop [
         current-state state
        ;;  last-winner-score nil
         ]
    (let [current-number (first (:numbers current-state))
          boards (:boards current-state)
          covered-boards (map #(cover-number % current-number) boards)
          winner-boards (filter #(is-winner %) covered-boards)
          maybe-winner (or (first winner-boards) :no-winner)
          new-boards (subtract covered-boards winner-boards)
          ]
      
      ;; (println last-winner-score)

      (println (str "covered boards: " (count covered-boards)))
      (println (count winner-boards))
      ;; (println (count  (:boards current-state)))
      ;; (println current-number)
      ;; (println (:boards current-state))

      (if (empty? new-boards)
        (calc-score maybe-winner (read-string current-number))
        (recur (->
                (update current-state :numbers #(rest %))
                (assoc :boards new-boards)))))))


(defn parse-board [board-string idx]
  (let [rows (->>
              (str/split board-string #"\n")
              (map #(str/split (str/trim %) #"\s+"))
              (vec))
        columns (util/transpose rows)]

    {:rows rows, :columns columns, :index idx}))

(defn parse-state [blocks]
  {:numbers (str/split (first blocks) #","), :boards (map-indexed #(parse-board %2 %1) (rest blocks))})


(run-games (parse-state (->
                         (slurp "/home/lukas/code/clojure/advent-of-code-2021/input.txt")
                         (str/split #"\n\n"))))

(parse-state (->
              (slurp "/home/lukas/code/clojure/advent-of-code-2021/input.txt")
              (str/split #"\n\n")))

(filter #(not= % [1 2 3]) [[1 2] [1 2 3] [5 6]])