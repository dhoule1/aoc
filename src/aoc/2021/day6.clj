(ns aoc.2021.day6
  (:require [clojure.string :as str]))


(def example "3,4,3,1,2")

(defn parse [input]
  (->> input
       (#(str/split % #","))
       (mapv #(Integer/parseInt (str/trim %)))
       frequencies))


(parse example)
(get (parse example) 3)


(defn progress-day [fish]
  (defn getn [m k]
    (let [v (get m k)]
      (if (nil? v) 0 v)))


  (reduce (fn[acc current]
            (let [k (key current)]
              (if (zero? k)
                (assoc
                 (assoc acc 6 (+ (val current) (getn acc 6)))
                 8 (+ (val current) (getn acc 8)))
                (assoc acc (dec k) (+ (getn acc (dec k)) (val current))))))
          {} fish))


(defn part1 [input]
  (reduce + (vals (nth (iterate progress-day (parse input)) 80))))


(def input (slurp "resources/aoc/2021/day6.txt"))

(comment
  (part1 example)
  (part1 input))



;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;

(defn part2 [input]
  (reduce + (vals (nth (iterate progress-day (parse input)) 256))))


(comment
  (part2 example)
  (part2 input)
  )
