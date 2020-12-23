(ns aoc.2020.day3
  (:require [clojure.string :as str]))

(def raw_input (slurp (clojure.java.io/reader "resources/aoc/2020/day3.txt")))

raw_input

(def parsed_input
  (->> raw_input
       str/split-lines
         (map seq)
         (map #(into [] %))
         (into [])))

(first parsed_input)

(count parsed_input)
(count (first parsed_input))

(defn lookup [m [x y]]
  (let [width (count (first m))]
    (nth (nth m y) (mod x width))))

(first parsed_input)

(def part1
  (loop [x 0 y 0 m parsed_input num_of_trees 0]
    (if (>= y (count m))
      num_of_trees
      (let [c (lookup m [x y])]
        (if (= c \#)
          (recur (+ 3 x) (+ 1 y) m (inc num_of_trees))
          (recur (+ 3 x) (+ 1 y) m num_of_trees)

          )))))


(defn count_trees [slope m]
  (let [{:keys [right down]} slope]
    (loop [x 0 y 0 m m num_of_trees 0]
      (if (>= y (count m))
        num_of_trees
        (let [c (lookup m [x y])]
          (if (= c \#)
            (recur (+ right x) (+ down y) m (inc num_of_trees))
            (recur (+ right x) (+ down y) m num_of_trees)
            ))))))

(count_trees {:right 3 :down 1} parsed_input)

(def part2
  (->>
    '({:right 1 :down 1},
      {:right 3 :down 1},
      {:right 5 :down 1},
      {:right 7 :down 1},
      {:right 1 :down 2},)
    (map #(count_trees % parsed_input))
    (reduce *)))


(comment
  (clojure.pprint/cl-format *out* "~{~{~a~}~%~}"
                            (->> "
  ...#...###......##.#..#.....##.
  ...#...###......##.#..#.....##.
  ...#...###......##.#..#.....##.
  ...#...###......##.#..#.....##.
  ...#...###......##.#..#.....##.
  ...#...###......##.#..#.....##.
  ...#...###......##.#..#.....##.
  ...#...###......##.#..#.....##.
  ...#...###......##.#..#.....##.
  "
                                 str/split-lines
                                 (map seq)
                                 (map #(into [] %))
                                 (into [])
                                 ))
  )