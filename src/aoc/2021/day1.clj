(ns aoc.2021.day1
  (:require [clojure.string :as str]))

(def test-input "199
200
208
210
200
207
240
269
260
263")

(defn parse [input]
  (->> input
      (str/split-lines)
      (map #(Integer/parseInt %))))

(def parsed-test-input (parse test-input))

(defn part1 [input]
  (->> input
       (partition 2 1)
       (filter (fn [[f s]] (> s f)))
       (count)))

(part1 parsed-test-input)



(def real-input (slurp "resources/aoc/2021/day1.txt"))

(def parsed-input (parse real-input))

(part1 parsed-input)
