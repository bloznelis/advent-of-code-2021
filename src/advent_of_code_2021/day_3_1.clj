(require '[advent-of-code-2021.utils :as util])

(defn power-consumption [gammab]
  (let [gamma-rate (util/to-decimal gammab)
        epsilon-rate (util/to-decimal (map #(case %
                                         1 0
                                         0 1) gammab))]
    (* gamma-rate epsilon-rate)))

(defn solve [input]
  (->>
   (util/tokenize input)
   (util/transpose)
   (map #(frequencies %))
   (map #(key (apply max-key val %)))
   (power-consumption)))