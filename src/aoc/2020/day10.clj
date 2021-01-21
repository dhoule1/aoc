(ns aoc.2020.day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")

(defn parse [input]
  (->> input
       str/split-lines
       (map #(Integer/parseInt %))
       sort
       )
  )

(def test-input-parsed (parse test-input))

test-input-parsed


(frequencies
  (map (fn [[x y]] (- y x))
       (partition 2 1
                  (concat [0] test-input-parsed [(+ 3 (apply max test-input-parsed))]))))


(defn device-voltage [adaptors]
  (+ 3 (apply max adaptors))
  )

(defn voltage-deltas [adaptors]
  (mapv (fn [[x y]] (- y x))
       (partition 2 1
                  (concat [0] adaptors [(device-voltage adaptors)])))
  )

(defn calculate-frequencies-of-voltages [adaptors]
  (frequencies
    (voltage-deltas adaptors))
  )


(calculate-frequencies-of-voltages test-input-parsed)


(def test-input-2 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

(def test-input-2-parsed (parse test-input-2))

(let [frequencies (calculate-frequencies-of-voltages test-input-2-parsed)]
  (* (get frequencies 1) (get frequencies 3))
  )

(def input (slurp (io/reader "resources/aoc/2020/day10.txt")))

(def parsed-input (parse input))

(def part1
  (let [frequencies (calculate-frequencies-of-voltages parsed-input)]
    (* (get frequencies 1) (get frequencies 3))))

part1


(voltage-deltas test-input-parsed)

[1 3 1 1 1 3 1 1 3 1 3 3]
[1 3   2 1 3 1 1 3 1 3 3]
[1 3     3 3 1 1 3 1 3 3]
[1 3 1   2 3 1 1 3 1 3 3]
[1 3 1 1 1 3   2 3 1 3 3]
[1 3   2 1 3   2 3 1 3 3]
[1 3     3 3   2 3 1 3 3]
[1 3 1   2 3   2 3 1 3 3]

(def count-ways
  (memoize (fn
             ([x] 1)
             ([x y & rest]
              (if (> (+ x y) 3)
                (apply count-ways y rest)
                (+
                  (apply count-ways y rest)
                  (apply count-ways (+ x y) rest)))))))

(def part2
  (apply count-ways (voltage-deltas parsed-input)))

part2

test-input-parsed

[1 4 5 6 7 10 11 12 15 16 19] []
[1 4 5 6 7 10 11 12 15 16] [1]
[1 4 5 6 7 10 11 12 15] [0 1]
[1 4 5 6 7 10 11 12] [0 0 1]
[1 4 5 6 7 10 11] [0 0 0 1]
[1 4 5 6 7 10] [1 0 0 0 1]
[1 4 5 6 7] [0 1 0 0 0 1]
[1 4 5 6] [0 0 1 0 0 0 1]
[1 4 5] [1 0 1 0 0 0 1]
[1 4] [3 1 0 1 0 0 0 1]

(voltage-deltas test-input-parsed)