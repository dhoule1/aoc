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



;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;


(defn part2-parse [input]
  (->> input
       str/split-lines
       (map seq)
       (map #(apply (fn [& chars] (map (fn [char] (Integer/parseInt (str char))) chars)) %))
       (map vec)
       vec))

(def part2-test-input-parsed (part2-parse test-input))

(defn input-range [input]
  (range (count (first input))))

(input-range part2-test-input-parsed)

(defn bits-by-index [input index]
  (nth (apply map vector input) index))

(bits-by-index part2-test-input-parsed 0)

(defn most-common-bit [input]
  (let [zeros (count (filter #{0} input))
        ones (count (filter #{1} input))]
    (if (>= ones zeros) 1 0)))

(defn least-common-bit [input]
  (let [zeros (count (filter #{0} input))
        ones (count (filter #{1} input))]
    (if (<= zeros ones) 0 1)))

(most-common-bit (bits-by-index part2-test-input-parsed 0))

(defn rotate [v]
  (apply map vector v))

(defn most-common-bits [input]
  (map (fn [column] (most-common-bit column))
       (rotate input)))

(defn least-common-bits [input]
  (map (fn [column] (least-common-bit column))
       (rotate input)))


(most-common-bits part2-test-input-parsed)
;; => (1 0 1 1 0)


(defn part2 [input]
  (defn calculate-rating [acquire-bit-fun]
    (let [range-of-input (range (count (first input)))
          resulting-bit
          (first
           (reduce (fn [acc bit-index]
                     (if (= 1 (count acc)) (reduced acc)
                         (filter
                          (fn [input-row]
                            (= (nth input-row bit-index)
                               (nth
                                (acquire-bit-fun acc)
                                bit-index
                                )))
                          acc)))
                   input
                   range-of-input))]
      (Integer/parseInt (apply str resulting-bit) 2)))

  (def oxygen-generator-rating (calculate-rating most-common-bits))
  (def co2-scrubber-rating (calculate-rating least-common-bits))

  (* oxygen-generator-rating co2-scrubber-rating))

part2-test-input-parsed


(part2 part2-test-input-parsed)

(def part2-input-parsed (part2-parse real-input))
(part2 part2-input-parsed)
