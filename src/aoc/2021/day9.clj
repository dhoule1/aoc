(ns aoc.2021.day9
  (:require [clojure.string :as str]))

(def example
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(def input (slurp "resources/aoc/2021/day9.txt"))

(let [input example
      lines (str/split-lines input)]
  (for [[i e] (map-indexed #([%1 %2]) lines)]
    [i e]))



(for [[y row] (map-indexed (fn [i e] [i e]) (str/split-lines example))
      [x num] (map-indexed (fn [i e] [i e]) (seq row))]
  num)





(defn parse [input]
  (vec(for [r (str/split-lines input)
            :let [c (seq r)]]
        (mapv (fn [x] (Integer/parseInt (str x))) c))))


(defn part1 [input]
  (defn adj [matrix [x y]]
    (for [[y' r] (map-indexed (fn [i e] [i e]) matrix)
          :when (or (= y' y)
                    (= y' (dec y))
                    (= y' (inc y)))
          [x' c] (map-indexed (fn [i e] [i e]) r)
          :when (or (= x' x)
                    (= x' (dec x))
                    (= x' (inc x)))
          :when (not= [x' y'] [x y])
          ]
      c) )


  (let [matrix (parse input)
        low-points
        (for [[y r] (map-indexed (fn [i e] [i e]) matrix)
              [x c] (map-indexed (fn [i e] [i e]) r)
              :let [adj-cells (adj matrix [x y])]
              :when (= (count adj-cells) (count (filter (fn [i] (> i c)) adj-cells)))]
          c
          )]
    (reduce + (map inc low-points))))

(part1 example)
(part1 input)



;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;




(defn part2 [input]
  (defn adj [matrix [x y]]
    (for [[y' r] (map-indexed (fn [i e] [i e]) matrix)
          :when (or (= y' y)
                    (= y' (dec y))
                    (= y' (inc y)))
          [x' c] (map-indexed (fn [i e] [i e]) r)
          :when (or (= x' x)
                    (= x' (dec x))
                    (= x' (inc x)))
          :when (and (not= [x' y'] [x y])
                     (or (= x' x) (= y' y)))]
      {[x' y'] c}) )


  (let [matrix (parse input)
        basins
        (for [[y r] (map-indexed (fn [i e] [i e]) matrix)
              [x c] (map-indexed (fn [i e] [i e]) r)
              :let [adj-cells (adj matrix [x y])]
              :when (= (count adj-cells) (count (filter (fn [i] (> (first (vals i)) c)) adj-cells)))]
          (loop [basin (filter (fn [x] (not= 9 (first (vals x)))) adj-cells)
                 collected #{{[x y] c}}]
            (if (empty? basin) collected
                (let [b (first basin)
                      addtl-basin (filter (fn [ab] (and (> (first (vals ab))
                                                           (first (vals b)))
                                                        (not= 9 (first (vals ab)))) )
                                          (adj matrix (first (keys b)))) _ ]
                  (recur (concat (rest basin) addtl-basin) (conj collected b))))))]
    (reduce (fn [acc b1] (* acc (count b1))) 1
            (take 3 (sort (fn [b1 b2] (> (count b1) (count b2))) basins)))
    ))

(count (nth
        (part2 example) 3))

(part2 example)
(part2 input)
