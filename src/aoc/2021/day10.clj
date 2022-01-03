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

((clojure.set/map-invert pairs) \])

(defn part2 [input]
  (let [closers (clojure.set/map-invert pairs)]
    (let [scores
          (for [line (parse input)
                :let [enriched (enrich line)]
                :when (not (:corrupted? enriched))]
            (let [complete-by (:rest
                               (reduce (fn [acc current]
                                         (if (closers current) (update acc :closers (fn [old] (conj old current)))
                                             (let [last-seen (peek (:closers acc))]
                                               (if (= last-seen (pairs current))
                                                 (update acc :closers (fn [old] (pop old)))
                                                 (update acc :rest (fn [old] (conj old (pairs current))))))))
                                       {:rest [] :closers []}
                                       (reverse line)))]
              (reduce
               (fn [score current]
                 (let [new-score (* score 5)]
                   (condp = current
                     \) (+ new-score 1)
                     \] (+ new-score 2)
                     \} (+ new-score 3)
                     \> (+ new-score 4))))
               0
               complete-by)))]
      (nth (sort scores) (Math/floor (/ (count scores) 2)) ))))


(part2 example)
(part2 input)
