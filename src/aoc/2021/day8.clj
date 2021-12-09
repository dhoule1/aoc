(ns aoc.2021.day8
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))


(def example "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")


(defn part1 [input]
  (->> input
       (str/split-lines)
       (map (fn [line] (second (first (re-seq #"\| ([a-g ]+)" line)))))
       (mapcat #(re-seq #"[a-g]+" %))
       (filter #(contains? #{2 3 4 7} (count %)))
       count))

(part1 example)


(def input (slurp "resources/aoc/2021/day8.txt"))


(part1 input)




;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;


;  aaaa
; b    c
; b    c
;  dddd
; e    f
; e    f
;  gggg

;; (comment
(defn parse [input]
  (for [line (str/split-lines input)
        :let [[signal-patterns output] (str/split line #"\|")]
        :let [i (str/split (str/trim signal-patterns) #"\s")]
        :let [o (str/split (str/trim output) #"\s")]]
    [(set i) (vec o)]
    )
  )

(first
 (first (parse example)))
;; => #{"cgeb" "edb" "fabcd" "agebfd" "fdcge" "be" "cbdgef" "fecdb" "fgaecd" "cfbegad"}


(second
 (first (parse example)))
;; => #{"fdgacbe" "gcbe" "cefdb" "cefbgd"}


(defn len [n input]
  (map (comp set seq) (filter #(= n (count %)) input)))

(defn calculate-entry [entry]
  (let [[i o] entry]
    (let [a (first (set/difference (first (len 3 i)) (first (len 2 i))))
          c (first (for [six (len 6 i)
                         :let [diff (set/difference (first (len 2 i)) six)]
                         :when (not (empty? diff))]
                     (first diff)))
          f (first (set/difference (first (len 2 i)) #{c}))
          b (first (set/difference
                    (set (for [five (len 5 i)
                               :let [diff (set/difference (first (len 4 i)) five)]
                               :when (not (empty? diff))]
                           (first diff)))
                    #{a c f}))
          d (first (set/difference (first (len 4 i)) #{a c f b}))
          g (first (for [five (len 5 i)
                         :let [diff (set/difference five #{a c f b d})]
                         :when (and (= 1 (count diff)) (nil? (#{a c f b d} (first diff))))]
                     (first diff)))
          e (first (set/difference (first (len 7 i)) #{a c f b d g}))
          cipher {a \a b \b c \c d \d e \e f \f g \g}]





      (def zero' #{\a \b \c \e \f \g})
      (def one' #{\c \f})
      (def two' #{\a \c \d \e \g})
      (def three' #{\a \c \d \f \g})
      (def four' #{\b \c \d \f})
      (def five' #{\a \b \d \f \g})
      (def six' #{\a \b \d \e \f \g})
      (def seven' #{\a \c \f})
      (def eight' #{\a \b \c \d \e \f \g})
      (def nine' #{\a \b \c \d \f \g})

      (def digits {
                   zero'  0
                   one'   1
                   two'   2
                   three' 3
                   four'  4
                   five'  5
                   six'   6
                   seven' 7
                   eight' 8
                   nine'  9
                   })





      (let [digit-seq
            (map (fn [scrambled]
                   (let [unscrambled
                         (set
                          (map (fn [wrong-letter]
                                 (get cipher wrong-letter)) (seq scrambled)))]
                     (println unscrambled)
                     (digits (first (filter (fn [d] (= d unscrambled)) (keys digits))))))  o)]
        (Integer/parseInt (str/join digit-seq)))
      )))


(calculate-entry (first (parse example)))


(defn part2 [input]
  (reduce +
          (for [entry (parse input)
                :let [n (calculate-entry entry)]]
            n)))


(part2 input)
