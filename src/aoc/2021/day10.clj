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
  {\[  \]
   \(  \)
   \{  \}
   \<  \>})

(pairs \[)

(defn parse [input]
  (->> input
       str/split-lines
       (map seq)))

(defn enrich [line]
  (reduce
   (fn [acc [i current]]
     (if (= i (dec (count line)))
       (reduced acc)
       (if (pairs current)
         (update acc :uncorrupted-stack (fn [old] (conj old current)))
         (let [previous (peek (:uncorrupted-stack acc))]
           (if (= current (pairs previous))
             (update acc :uncorrupted-stack (fn [old] (pop old)))
             (reduced {:corrupted? true :index-of-corruption i :uncorrupted-stack (:uncorrupted-stack acc)}))))))
   {:corrupted? false :index-of-corruption nil :uncorrupted-stack []}
   (map-indexed (fn [i c] [i c]) line)))

(enrich (nth (parse example) 1))


(defn part1 [input]
  (let [incorrect-chars
        (for [line (parse input)
              :let [enriched (enrich line)]
              :when (:corrupted? enriched)]
          (nth line (:index-of-corruption enriched)))]
    (reduce
     (fn [acc x]
       (+ acc (condp = x \) 3 \] 57 \} 1197 \> 25137)))
     0 incorrect-chars)))

(part1 example)
(part1 input)

;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;
