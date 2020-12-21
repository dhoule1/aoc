(ns aoc.2020.day1)

(def raw_input
  (slurp (clojure.java.io/resource "aoc/2020/day1.txt")))

(def num_input
  (->> raw_input
       clojure.string/split-lines
       (map #(Integer/parseInt %))))

(def part_1
  (first
    (into #{}
          (for [x num_input
                y num_input
                :when (= 2020 (+ x y))]
            (* x y)))))

(def part_2
  (first
    (into #{}
          (for [x num_input
                y num_input
                z num_input
                :when (= 2020 (+ x y z))]
            (* x y z)))))
