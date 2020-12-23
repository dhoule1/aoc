(ns aoc.2020.day4
  (:require [clojure.string :as str]))

(def sample_raw_input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")

(defn parse_data_element
     [data_str]
     (->> data_str
          (re-seq #"(\w+):(\S+)")
          first
          rest
          ((fn [[x y]]
                {:key x :value y}))))

(defn parse [input]
     (->> input
          (#(str/split % #"\n\n"))
          (map str/split-lines)
          (map (fn [[& args]]
                    (reduce (fn [e1 e2]
                                 (str e1 " " e2)) args)))
          (map #(str % " "))
          (map #(re-seq #"\w+:\S+\s+" %))
          (map (fn [[& x]] (map str/trim x)))
          (map (fn [[& x]] (map parse_data_element x)))
          ) )

(def sample_parsed_input
     (parse sample_raw_input))

sample_parsed_input


(defn has_required_fields? [passport]
     (let [[& fields] passport]
          (> (count (filter (fn [{:keys [key]}] (not= key "cid")) fields))
             6)
          ))
(first sample_parsed_input)

(has_required_fields?
     (nth sample_parsed_input 3))

(count
     (filter has_required_fields? sample_parsed_input))

(def raw_input (slurp (clojure.java.io/reader "resources/aoc/2020/day4.txt")))

(def parsed_input (parse raw_input))

(def part1
     (count (filter has_required_fields? parsed_input)))

(defn find_value [k fields]
     (if-let [field
              (first (filter (fn [field] (= k (:key field))) fields))]
          (:value field) false))

(first parsed_input)


(defn valid_birth_year? [passport]
     (if-let [value (find_value "byr" passport)]
          (let [val_num (Integer/parseInt value)]
               (and
                    (= 4 (count value))
                    (>= val_num 1920)
                    (<= val_num 2002)
                    )), false))

(defn valid_issue_year? [passport]
     (if-let [value (find_value "iyr" passport)]
          (let [val_num (Integer/parseInt value)]
               (and
                    (= 4 (count value))
                    (>= val_num 2010)
                    (<= val_num 2020)
                    )), false))

(defn valid_expiration_year? [passport]
     (if-let [value (find_value "eyr" passport)]
          (let [val_num (Integer/parseInt value)]
               (and
                    (= 4 (count value))
                    (>= val_num 2020)
                    (<= val_num 2030)
                    )), false))

(defn valid_height? [passport]
     (if-let [value (find_value "hgt" passport)]
          (if-let [[_ height measure] (re-matches #"^(\d+)(cm|in)$" value)]
               (let [height_num (Integer/parseInt height)]
                    (if (= measure "cm")
                         (and
                              (>= height_num 150)
                              (<= height_num 193)
                              )
                         (and
                              (>= height_num 59)
                              (<= height_num 76)
                              )))
               false), false))


(defn valid_hair_color? [passport]
     (if-let [value (find_value "hcl" passport)]
          (some? (re-matches #"#[0-9a-f]{6}" value))
          false))

(defn valid_eye_color? [passport]
     (if-let [value (find_value "ecl" passport)]
          (some? (re-matches #"amb|blu|brn|gry|grn|hzl|oth" value))
          false))

(defn valid_passport_Id? [passport]
     (if-let [value (find_value "pid" passport)]
          (some? (re-matches #"\d{9}" value))
          false))

(valid_height? '({:key "hgt" :value "76in"}))
(valid_birth_year? '({:key "byr" :value "2002"}))
(valid_hair_color? '({:key "hcl" :value "#abcdef"}))
(valid_eye_color? '({:key "ecl" :value "wat"}))
(valid_passport_Id? '({:key "pid" :value "0123456789"}))


(def part2
     (->> parsed_input
          (filter
               (apply every-pred [
                                  has_required_fields?
                                  valid_birth_year?
                                  valid_issue_year?
                                  valid_expiration_year?
                                  valid_height?
                                  valid_hair_color?
                                  valid_eye_color?
                                  valid_passport_Id?
                                  ]))
          count))

part2
