(ns aoc.2021.day7
  (:require [clojure.string :as str]))

(def example "16,1,2,0,4,2,7,1,2,14")

(defn parse [input]
  (-> input
      (str/split #",")
      (#(mapv (fn[x] (Integer/parseInt (str/trim x))) %))))

(def input (slurp "resources/aoc/2021/day7.txt"))

(parse example)


(defn part1 [input_raw]
  (let [input (parse input_raw)
        min_pos (reduce min input)
        max_pos (reduce max input)]
    (second
     (reduce (fn [[_ v1 :as m1] [_ v2 :as m2]] (if (> v1 v2) m2 m1))
             (for [destination (range min_pos (inc max_pos))]
               [destination (reduce + (map (fn [i] (Math/abs (- i destination))) input))])))))

(comment
  (part1 example)
  (part1 input))



;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;


(defn part2 [input_raw]
  (defn calculate-fuel-cost [origin destination]
    (Math/abs (- origin destination)))

  (let [input (parse input_raw)
        min_pos (reduce min input)
        max_pos (reduce max input)]
    (second
     (reduce (fn [[_ v1 :as m1] [_ v2 :as m2]] (if (> v1 v2) m2 m1))
             (for [destination (range min_pos (inc max_pos))]
               [destination (reduce + (map (fn [i]
                                             (let [steps (Math/abs (- i destination))]
                                               (+ steps (reduce + (range steps))))) input))])))))


(comment
  (part2 example)
  (part2 input))
