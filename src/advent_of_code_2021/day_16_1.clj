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
       int))

(defn read-literal-packet [bits]
  (loop [remaining-zipped-bits bits
         acc []]
    (let [head (first remaining-zipped-bits)
          bit-group-with-tail (split-at 5 remaining-zipped-bits)]
      (if (= head \0)
        {:packet (conj acc (rest (first bit-group-with-tail))) :remaining-bits (second bit-group-with-tail)}
        (recur (second bit-group-with-tail) (conj acc (rest (first bit-group-with-tail))))))))

(declare operator-bits)

(defn read-packet [bits]
  (let [version-bits-with-tail (split-at 3 bits)
        id-bits-with-tail (split-at 3 (second version-bits-with-tail))
        id-decimal (group-decimal-value (first id-bits-with-tail))
        version-decimal (group-decimal-value (first version-bits-with-tail))]

    (case id-decimal
      4 {:result version-decimal, :remaining (:remaining-bits (read-literal-packet (second id-bits-with-tail)))}
      (operator-bits (second id-bits-with-tail) version-decimal))))

(defn operator-bits [bits version-decimal]
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
                 {:result (conj acc version-decimal), :remaining (second sub-packet-bits-with-tail)}
                 (let [literal-packet-read-result (read-packet remaining-bits)]
                   (recur (:remaining literal-packet-read-result) (conj acc (:result literal-packet-read-result))))))
      \1 (loop [remaining-bits (second length-bits-with-tail)
                acc []
                remaining-sub-packets-amount length-decimal]
           (if (= 0 remaining-sub-packets-amount)
             {:result (conj acc version-decimal), :remaining remaining-bits}
             (let [literal-packet-read-result (read-packet remaining-bits)]
               (recur (:remaining literal-packet-read-result) (conj acc (:result literal-packet-read-result)) (dec remaining-sub-packets-amount))))))))

(defn solve [input]
  (->> input
       read-hex
       read-packet
       :result
       flatten
       (apply +)))