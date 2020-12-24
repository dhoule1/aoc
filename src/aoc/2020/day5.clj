(ns aoc.2020.day5
  (:require [clojure.string :as str]))

(def test-input "FBFBBFFRLR")

(defn binary-search [binary-seq default-min default-max lower-bounds ]
  (loop [s binary-seq
         min default-min
         max default-max]
    (let [char (first s)]
      (if (= 1 (Math/abs (- max min)))
        (if (= char lower-bounds) min max)

        (if (= char lower-bounds)
          (recur (rest s) min (/ (+ (- max 1) min) 2))
          (recur (rest s) (/ (+ (+ max 1) min) 2) max)

          ))))
  )
(defn determine-row [boarding-pass]
  (let [min-rows 0
        max-rows 127
        front \F
        binary-seq (take 7 (seq boarding-pass))]
    (binary-search binary-seq min-rows max-rows front)
    ))


(determine-row test-input)


(defn determine-column [boarding-pass]
  (let [min-columns 0
        max-columns 7
        front \L
        binary-seq (take-last 3 (seq boarding-pass))]
    (binary-search binary-seq min-columns max-columns front)
    ))

(determine-column test-input)

(defn determine-seat-id [boarding-pass]
  (let [row (determine-row boarding-pass)
        column (determine-column boarding-pass)]
    (+ (* 8 row) column)))

(determine-seat-id test-input)
(determine-seat-id "BFFFBBFRRR")
(determine-seat-id "FFFBBBFRRR")
(determine-seat-id "BBFFBBFRLL")


(def raw-input (slurp (clojure.java.io/reader "resources/aoc/2020/day5.txt")))

(def part1
  (->> raw-input
       str/split-lines
       (map determine-seat-id)
       (sort (comp - compare))
       first))

part1

(def part2-parsed-input
  (->> raw-input
       str/split-lines
       (map (fn [bp] {:row (determine-row bp) :column (determine-column bp)}))))

(take 1 part2-parsed-input)

(def available-seats
  (for [row (range 1 127)
        column (range 0 7)
        :when (empty? (filter #{{:row row :column column}} part2-parsed-input))]
    [row column]))

(count available-seats)

(def available-seat-ids
  (->> available-seats
       (map (fn [[row column]] (+ (* 8 row) column)))))

(def boarding-pass-ids
  (->> part2-parsed-input
       (map (fn [{:keys [row column]}] (+ (* 8 row) column)))))

(sort boarding-pass-ids)

(for [open-id available-seat-ids
      taken-id boarding-pass-ids
      :when (or
              (= (+ open-id 1) taken-id)
              (= (- open-id 1) taken-id)
              )]
  taken-id
  )