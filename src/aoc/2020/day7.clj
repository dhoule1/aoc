(ns aoc.2020.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def test-input "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")


(defn extract-bag-identifier [line]
  (let [[kw1 kw2] (re-seq #"[a-zA-Z]+" line)
        r (keyword (str kw1 "-" kw2))]
    r))


(defn extract-quantity [content]
  (Integer/parseInt
    (str
      (first
        (re-seq #"^\d+" content))), 10))

(defn parse-line [line]
  (let [[container & contains]
        (remove #{"no other bag"}
                (map second
                     (re-seq #"(((\d\W)*\w+\W\w+) bag)+" line)))]
    (let [container-id (extract-bag-identifier container)
          content-ids (map extract-bag-identifier contains)
          content-quantities (map extract-quantity contains)]
      {container-id (apply zipmap
                           (map (fn [[& all]] (into [] all))
                                [content-ids content-quantities]))})))

(def test-rules
  (->> test-input
       str/split-lines
       (map parse-line)))

(defn contains-bag? [bag rule]
  (let [bag-contains (first (vals rule))]
    (contains? bag-contains bag)))

(contains-bag? :bright-white (first test-rules))

(->> test-rules
     (filter #(contains-bag? :shiny-gold %))
     (map keys)
     (map first)
     )

(defn subject [rule]
  (first (keys rule))
  )
(defn rule [rules subject]
  (first (filter (fn [r] (= subject (first (keys r)))) rules)))

(rule test-rules
  (subject
    (first test-rules)))

(first test-rules)
(defn rules-containing [rules bag]
  (filter (fn [rule] (contains? (first (vals rule)) bag)) rules))

(defn rule-contains [rule]
  (first (vals rule)))

(rule-contains (first test-rules))

(rules-containing test-rules :bright-white)

(defn find-all-containing-bags
  ([bag rules] (remove #{bag} (find-all-containing-bags rules #{bag} #{})))
  ([rules already-found processed]
   (if-let [bag (first already-found)]
     (if-let [found-rules (rules-containing rules bag)]
       (if-let [new-rules (set/difference (into #{} found-rules) (into #{} already-found))]
         (recur
           rules
           (concat (rest already-found) (map subject new-rules))
           (conj processed bag))
         (recur
           rules
           (rest already-found)
           (conj processed bag)))
       processed)
     processed)
   ))

(find-all-containing-bags :shiny-gold test-rules)


(def input (slurp (io/reader "resources/aoc/2020/day7.txt")))

(def rules
  (->> input
       str/split-lines
       (map parse-line)))

(def part1
  (count
    (find-all-containing-bags :shiny-gold rules)))

part1


(defn calculate-total-bags-within
  ([bag rules] (calculate-total-bags-within bag rules 1))
  ([bag rules multiplier]
   (let [contents (rule-contains (rule rules bag))]
     (+ (* multiplier (reduce + (vals contents)))
        (reduce + (map
                    #(calculate-total-bags-within % rules (* multiplier (% contents))) (keys contents))))
     ))
  )

(calculate-total-bags-within :shiny-gold test-rules)

(def test-input-2 "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.")

(def test-rules-2
  (->> test-input-2
       str/split-lines
       (map parse-line)))

(calculate-total-bags-within :shiny-gold test-rules-2)

(def part2
  (calculate-total-bags-within :shiny-gold rules))

part2