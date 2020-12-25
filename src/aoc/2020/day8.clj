(ns aoc.2020.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")

(defn parse-line [i line]
  (let [[[_ operation sign value]] (re-seq #"^(\w{3}) (\+|\-)(\d+)$" line)]
    {:index i :operation operation :sign sign :value (Integer/parseInt value)})
  )

(defn parse [input]
  (->> input
       str/split-lines
       (map-indexed parse-line)
       (into [])))

(def parsed-test-input
  (parse test-input))

(defn find-instruction-by-index [i input]
  (first (filter #(= (:index %) i) input)))

(defn init-move [instructions]
  (fn
    ([instruction] (find-instruction-by-index (+ 1 (:index instruction)) instructions))
    ([instruction index] (find-instruction-by-index index instructions))
    ))

(defn run-boot-code [instructions]
  (let [move-next (init-move instructions)]
    (loop [instruction (first instructions)
           accumulator 0
           history #{}]
      (if (and instruction (not (contains? history (:index instruction))))
        (let [op (:operation instruction)
              sign (:sign instruction)
              value (:value instruction)
              index (:index instruction)]
          (condp = op
            "nop" (recur (move-next instruction) accumulator (conj history index))
            "acc" (recur (move-next instruction) (if (= "+" sign)
                                                   (+ accumulator value)
                                                   (- accumulator value)) (conj history index))
            "jmp" (recur (move-next instruction (if (= "+" sign)
                                                  (+ index value)
                                                  (- index value))) accumulator (conj history index))))
        accumulator))))

(contains? #{1 2 3} 4)
(conj #{1 2 3} 4)

(first parsed-test-input)
(second parsed-test-input)
(nth parsed-test-input 8)
(run-boot-code '(
                 {:index 0, :operation "nop", :sign "+", :value 0}
                 {:index 1, :operation "acc", :sign "+", :value 1}
                 {:index 2, :operation "jmp", :sign "+", :value 4}
                 {:index 3, :operation "acc", :sign "+", :value 3}
                 {:index 4, :operation "jmp", :sign "-", :value 3}
                 {:index 5, :operation "acc", :sign "-", :value 99}
                 {:index 6, :operation "acc", :sign "+", :value 1}
                 {:index 7, :operation "jmp", :sign "-", :value 4}
                 {:index 8, :operation "acc", :sign "+", :value 6}
                 ))


(def input (slurp (io/reader "resources/aoc/2020/day8.txt")))

(def parsed-input (parse input))

(def part1
  (->> parsed-input
       run-boot-code))

part1

(defn find-all [o boot-code]
  (filter (fn [{:keys [operation]}] (= operation o) ) boot-code))
(defn find-all-nops [boot-code]
  (find-all "nop" boot-code))
(defn find-all-jmps [boot-code]
  (find-all "jmp" boot-code))

(find-all-nops parsed-test-input)
(find-all-jmps parsed-test-input)

(defn swap-instructions [instruction boot-code]
  (let [index (:index instruction)]
    (map
      (fn [line]
        (if (= (:index line) index) instruction line))
      boot-code))
  )

(first parsed-test-input)
(first
  (swap-instructions {:index 0 :operation "jmp" :sign "+" :value 0} parsed-test-input))




(defn run-boot-code-part2 [instructions]
  (let [move-next (init-move instructions)]
    (loop [instruction (first instructions)
           accumulator 0
           history #{}]
      (cond
        (nil? instruction) accumulator
        (contains? history (:index instruction)) (throw (new RuntimeException "Infinite Loop!!"))
        :else
        (let [op (:operation instruction)
              sign (:sign instruction)
              value (:value instruction)
              index (:index instruction)]
          (condp = op
            "nop" (recur (move-next instruction) accumulator (conj history index))
            "acc" (recur (move-next instruction) (if (= "+" sign)
                                                   (+ accumulator value)
                                                   (- accumulator value)) (conj history index))
            "jmp" (recur (move-next instruction (if (= "+" sign)
                                                  (+ index value)
                                                  (- index value))) accumulator (conj history index))))))))

(try
  (run-boot-code-part2 parsed-test-input)
  (catch RuntimeException e (str "caught exception " (.getMessage e)))
  )

(defn alter-operation [instruction new-operation]
  (let [{:keys [sign value index]} instruction]
    {:sign sign :value value :index index :operation new-operation})
  )

(defn calculate-part2 [boot-code]
  (let [jmps (find-all-jmps boot-code)]
    (defn execute-code [index]
      (let [instruction (nth jmps index)
            new-program (swap-instructions (alter-operation instruction "nop") boot-code)]
        (try (run-boot-code-part2 new-program)
             (catch RuntimeException e (execute-code (inc index)))))
      )
    (execute-code 0)
    )
  )

(calculate-part2 parsed-test-input)
(calculate-part2 parsed-input)

