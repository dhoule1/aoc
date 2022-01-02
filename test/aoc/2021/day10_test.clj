(ns aoc.2021.day10-test
  (:require [aoc.2021.day10 :as day10]
            [clojure.test :refer [deftest is]]))

(deftest part1-test
  (let [part1 day10/part1]
    (is (= 26397 (part1 day10/example)))
    (is (= 268845 (part1 day10/input)))))
