(ns aoc.2021.day2
  (:require [clojure.string :as str]))

(def test-input "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #"\s"))
       (map (fn [[dir amount]]
              {:direction (keyword dir) :amount (Integer/parseInt amount)}))
       )
)

(parse test-input)

(defn part1 [parsed-input]
  (loop [directions parsed-input
         {:keys [x y]} {:x 0 :y 0}]
    (if-let [current-dir (first directions)]
      (let [{:keys [direction amount]} current-dir]
        (println x y direction amount)
        (case direction
          :forward (recur (rest directions) {:x (+ x amount) :y y})
          :down (recur (rest directions) {:x x :y (+ y amount)})
          :up (recur (rest directions) {:x x :y (- y amount)})
          (recur (rest directions) {:x x :y y})))
      (* x y)))
  )
  

(part1 (parse test-input))

(def real-input (slurp "resources/aoc/2021/day2.txt"))

(def parsed-input (parse real-input))

(part1 parsed-input)
