(ns aoc.2021.day10
  (:require [clojure.string :as str]))

(def example
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(def input (slurp "resources/aoc/2021/day10.txt"))

(def pairs
  {
   \[  \]
   \(  \)
   \{  \}
   \<  \>
   })


(pairs \[)



(defn parse [input]
  (->> input
       str/split-lines
       (map seq)))

(defn part1 [input]
  (let [incorrect-chars
        (keep identity
              (for [line (parse input)]
                (let [result
                      (reduce
                       (fn [acc current]
                         (if-let [r (pairs current)]
                           (do
                             (.push acc current)
                             acc)
                           (let [previous (.peek acc)]
                             (if (= current (pairs previous))
                               (do
                                 (.pop acc)
                                 acc)
                               (do
                                 (reduced current))))))
                       (java.util.ArrayDeque.)
                       line)]
                  (if (char? result) result nil))))]
    (reduce
     (fn [acc x]
       (+ acc (condp = x \) 3 \] 57 \} 1197 \> 25137 )))
     0 incorrect-chars)))


(part1 example)
(part1 input)
