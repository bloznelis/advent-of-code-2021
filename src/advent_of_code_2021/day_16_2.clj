(require '[clojure.string :as str])
(require '[advent-of-code-2021.utils :as util])

(defn read-hex [hex-string]
  (str/join (map #(case %1
                    \0 "0000"
                    \1 "0001"
                    \2 "0010"
                    \3 "0011"
                    \4 "0100"
                    \5 "0101"
                    \6 "0110"
                    \7 "0111"
                    \8 "1000"
                    \9 "1001"
                    \A "1010"
                    \B "1011"
                    \C "1100"
                    \D "1101"
                    \E "1110"
                    \F "1111") hex-string)))

(defn group-decimal-value [group]
  (->> group
       (map str)
       (map read-string)
       util/to-decimal
       long))

(defn read-literal-packet [bits]
  (loop [acc []
         remaining-zipped-bits bits]
    (let [head (first remaining-zipped-bits)
          bit-group-with-tail (split-at 5 remaining-zipped-bits)]

      (if (= head \0)
        {:result (->> bit-group-with-tail
                      first
                      rest
                      (conj acc)
                      (apply concat)
                      group-decimal-value)
         :remaining (second bit-group-with-tail)}
        (recur  (->> bit-group-with-tail
                     first
                     rest
                     (conj acc)) (second bit-group-with-tail))))))

(declare operator-bits)

(defn read-packet [bits]
  (let [version-bits-with-tail (split-at 3 bits)
        id-bits-with-tail (split-at 3 (second version-bits-with-tail))
        id-decimal (group-decimal-value (first id-bits-with-tail))]

    (defn op [fn]
      (as-> (operator-bits (second id-bits-with-tail)) result
        (assoc result :result (fn (:result result)))))

    (case id-decimal
      4 (read-literal-packet (second id-bits-with-tail))
      0 (op #(apply + %))
      1 (op #(apply * %))
      2 (op #(apply min (flatten %)))
      3 (op #(apply max (flatten %)))
      5 (op #(if (> (first %) (second %)) 1 0))
      6 (op #(if (< (first %) (second %)) 1 0))
      7 (op #(if (= (first %) (second %)) 1 0)))))

(defn operator-bits [bits]
  (let [length-type-id-with-tail (split-at 1 bits)
        length-bit (first (first length-type-id-with-tail))
        length-bits-with-tail (if (= \0 length-bit)
                                (split-at 15 (second length-type-id-with-tail))
                                (split-at 11 (second length-type-id-with-tail)))
        length-decimal (group-decimal-value (first length-bits-with-tail))  ;; 27
        sub-packet-bits-with-tail (split-at length-decimal (second length-bits-with-tail))]

    (case length-bit
      \0     (loop [remaining-bits (first sub-packet-bits-with-tail)
                    acc []]
               (if (empty? remaining-bits)
                 {:result acc, :remaining (second sub-packet-bits-with-tail)}
                 (let [literal-packet-read-result (read-packet remaining-bits)]
                   (recur (:remaining literal-packet-read-result) (conj acc (:result literal-packet-read-result))))))
      \1 (loop [remaining-bits (second length-bits-with-tail)
                acc []
                remaining-sub-packets-amount length-decimal]
           (if (= 0 remaining-sub-packets-amount)
             {:result acc, :remaining remaining-bits}
             (let [literal-packet-read-result (read-packet remaining-bits)]
               (recur (:remaining literal-packet-read-result) (conj acc (:result literal-packet-read-result)) (dec remaining-sub-packets-amount))))))))

(defn solve [input]
  (->> input
       read-hex
       read-packet
       :result))

(def test-input "9C0141080250320F1802104A08")
(def real-input (slurp "/home/lukas/code/clojure/advent-of-code-2021/input.txt"))

(solve real-input)

