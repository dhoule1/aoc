(ns aoc.2020.day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def test-input "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

(defn parse [input]
  (->> input
       (#(str/split % #"\n\n"))
       (map set)
       (map #(remove #{\newline} %))
       ))

(def test-input-parsed
  (parse test-input))

test-input-parsed

(reduce +
        (map count test-input-parsed))

(def input (slurp (io/reader "resources/aoc/2020/day6.txt")))

(def part1
  (->> input
       parse
       (map count)
       (reduce +))
  )

part1


(defn determine-common-answers [group]
  (apply set/intersection (map set group)))

(->>
  '((\a) (\b \a) (\c \a))
  determine-common-answers
  )

(set '((\a) (\b)))

(defn part2-parse [input]
  (->> input
       (#(str/split % #"\n\n"))
       (map #(str/split-lines %))
       (map #(map seq %))
       ))

test-input
(part2-parse test-input)

(def part2
  (->> input
       part2-parse
       (map determine-common-answers)
       (map count)
       (reduce +)))

part2
