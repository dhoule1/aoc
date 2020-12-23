(ns aoc.2020.day2)

(def raw_input
  (slurp (clojure.java.io/reader "resources/aoc/2020/day2.txt")))

(defn parse [input_line]
  (let [[_ min max letter input]
        (re-matches #"^(\d+)-(\d+) (\S): (.+)$" input_line)]
    {
     :min    (Integer/parseInt min)
     :max    (Integer/parseInt max)
     :letter (first letter)
     :input  input})
  )

(def parsed_input (->> raw_input
                       clojure.string/split-lines
                       (map #(parse %))))

(defn valid_password? [{:keys [min max letter input]}]
  (let [lst (seq input)
        matches (filter #{letter} lst)
        count (count matches)
        valid (and (>= count min) (<= count max))]
    valid))

(def part1
  (count
    (filter valid_password? parsed_input)))

part1

(defn part2_parse [input_line]
  (let [[_ min max letter input]
        (re-matches #"^(\d+)-(\d+) (\S): (.+)$" input_line)]
    {
     :first_pos  (- (Integer/parseInt min) 1)
     :second_pos (- (Integer/parseInt max) 1)
     :letter     (first letter)
     :input      input})
  )

(def part2_parsed_input
  (->> raw_input
       clojure.string/split-lines
       (map #(part2_parse %))))

(first part2_parsed_input)

(defn part2_valid_password? [{:keys [first_pos second_pos letter input]}]
  (let [lst (seq input)
        valid (let [f (nth lst first_pos)
                    s (nth lst second_pos)] (and (or
                                                   (= letter f)
                                                   (= letter s))
                                                 (not= f s)))]
    valid))

(def part2
  (count
    (filter part2_valid_password? part2_parsed_input)))

part2