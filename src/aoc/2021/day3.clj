(ns aoc.2021.day3
  (:require [clojure.string :as str]))

(def test-input "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")


(defn parse [input]
  (->> input
       (str/split-lines)))

(def parsed-test-input (parse test-input))
parsed-test-input

(defn part1 [input]
  (defn to-decimal [binary-seq]
    (let [binary-str (apply str binary-seq)]
      (Integer/parseInt binary-str 2)))
  (defn flip [gamma]
    (map (fn [i] (if (= i 0) 1 0)) gamma))
  
  (let [gamma-seq  (->> input
                    (map seq)
                    (apply map (fn [& args] (frequencies args)))
                    (map (fn [f] (if (> (get f \0) (get f \1)) 0 1 )))
                    )]
    (let [gamma (to-decimal gamma-seq)
          epsilon (to-decimal (flip gamma-seq))]
      (* gamma epsilon))))

(part1 parsed-test-input)

(def real-input (slurp "resources/aoc/2021/day3.txt"))
(def parsed-input (parse real-input))

(part1 parsed-input)
