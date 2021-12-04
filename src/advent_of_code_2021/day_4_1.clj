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

(defn is-winner [board]
  (or (some? (some empty? (:rows board))) (some? (some empty? (:columns board)))))

(defn run [state]
  (loop [current-state state]
    (let [current-number (first (:numbers current-state))
          covered-boards (map #(cover-number % current-number) (:boards current-state))
          winner-board (first (filter #(is-winner %) covered-boards))]

      (case winner-board
        nil (recur (->
                    (update current-state :numbers #(rest %))
                    (assoc :boards covered-boards)))
        (calc-score winner-board (read-string current-number))))))

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