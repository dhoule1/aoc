(ns aoc.2020.day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576")

(def test-preamble-length 5)

(defn parse [input]
  (->> input
       str/split-lines
       (map #(Long/parseLong %))))

(def test-input-parsed
  (parse test-input))

test-input-parsed

(defn find-first-invalid-number [input preamble-length]
  (let [preamble (take preamble-length input)
        others (drop preamble-length input)]
    (loop [pre preamble post others]
      (let [subject (first post)]
        (if-let [valid (first
                         (for [x pre
                               y pre
                               :when (and
                                       (not= x y)
                                       (= subject (+ x y)))]
                           subject))]
          (recur (concat (rest pre) [subject]) (rest post))
          subject)
        )
      )
    ))

test-input-parsed
(find-first-invalid-number test-input-parsed test-preamble-length)

(def input (slurp (io/reader "resources/aoc/2020/day9.txt")))

(def input-parsed (parse input))


(def part1
  (find-first-invalid-number input-parsed 25))

part1

(count input-parsed)

test-input-parsed

(partition (count test-input-parsed) test-input-parsed)



(defn find-continuous-sequence [input invalid-num]
  (first
    (filter (comp not empty?)
            (for [i (range (count input) 1 -1)]
              (let [candidates (partition i 1 input)]
                (first
                  (for [candidate candidates
                        :when (= invalid-num (reduce + candidate))]
                    candidate)))))))

(let [r (find-continuous-sequence test-input-parsed 127)]
  (let [
        max (apply max r)
        min (apply min r)
        ]
    (+ max min)
    )
  )

(def part2
  (let [r (find-continuous-sequence input-parsed part1)]
    (let [
          max (apply max r)
          min (apply min r)
          ]
      (+ max min)
      )
    ))

part2