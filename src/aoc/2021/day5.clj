(ns aoc.2021.day5
  (:require [clojure.string :as str]))

(def example
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def input (slurp "resources/aoc/2021/day5.txt"))

(defn build-matrix [width height]
  (make-array Long/TYPE (inc width) (inc height)))


(defn update-matrix [matrix line]
  (for [[x y] line
        :let [current-value (aget matrix x y)]]
    (do (aset matrix x y (inc current-value))))
  )



;; [0 0] [0 5]
;; [[0 0] [0 1] [0 2] [0 3] [0 4] [0 5]]

;; [0 0] [1 1]
;; [[0 0] [1 1]]

;; [0 1] [1 0]
;; [[0 1] [1 0]]

;; [0 2] [2 0]
;; [[0 2] [1 1] [2 0]]

(defn build-line [[[xs ys] [xe ye]]]
  (loop [x xs
         y ys
         pairs [[xs ys]]]
    (if (and (= x xe) (= y ye)) pairs
        (let [next-x (if (< xe xs) (dec x) (if (> xe xs) (inc x) x))
              next-y (if (< ye ys) (dec y) (if (> ye ys) (inc y) y))]
          (recur next-x next-y (conj pairs [next-x next-y])
                 )))))


(build-line [[2 2] [2 1]])



(defn part1
  ([] (part1 input))
  ([input]
   (let [lines (str/split-lines input)
         coord-pairs-str (mapv (fn [line] (str/split line #" -> ")) lines)
         coord-pairs (filterv (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))) (mapv (fn [pair] (mapv (fn [p] (mapv #(Integer/parseInt %) (str/split p #","))) pair)) coord-pairs-str))
         max-x (apply max (for [[[x]] coord-pairs] x))
         max-y (apply max (for [[[_ y]] coord-pairs] y))
         matrix (build-matrix max-x max-y)]
     (doseq [coord-pair coord-pairs
             [x y] (build-line coord-pair)
             :let [val (aget matrix x y)]]
       (aset matrix x y (inc val)))
     (count (for [r (seq matrix)
                  c (seq r)
                  :when (>= c 2)]
              c))
     )
   ))





;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;


(defn part2
  ([] (part2 input))
  ([input]
   (let [lines (str/split-lines input)
         coord-pairs-str (mapv (fn [line] (str/split line #" -> ")) lines)
         coord-pairs (mapv (fn [pair] (mapv (fn [p] (mapv #(Integer/parseInt %) (str/split p #","))) pair)) coord-pairs-str)
         max-x (apply max (for [[[x]] coord-pairs] x))
         max-y (apply max (for [[[_ y]] coord-pairs] y))
         matrix (build-matrix max-x max-y)]
     (loop [coord-pairs coord-pairs
            matrix matrix]
       (let [coord-pair (first coord-pairs)]
         (if (not coord-pair)
           (count (filter #(>= % 2) (vals matrix)))
           (let [[[x1 y1] [x2 y2]] coord-pair]
             (let [line (build-line coord-pair)]
               (recur (rest coord-pairs) (update-matrix matrix line))))))
       ))
   ))




(comment
  (part1 example)
  (part1)
  (part2 example)
  (part2)
)
