(ns aoc.2021.day4
  (:require [clojure.string :as str]))

(def example
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(def input (slurp "resources/aoc/2021/day4.txt"))



(defn ->board [board-str]
  (-> board-str
      str/split-lines
      (#(mapv (fn [line] (mapv (fn [n] {:value (Integer/parseInt n) :hit false}) (remove #{""} (str/split line #" ")))) %))))

(defn update-board [number]
  (fn [board]
    (mapv (fn [line]
           (mapv (fn [cell]
                  (if (= (:value cell) number)
                    (assoc cell :hit true)
                    cell))  line))  board)))

(defn print-board [board]
  (clojure.pprint/pprint (vec (for [line board]
                  (mapv #(:value %) line)))))

(defn has-bingo? [board]
  (or
   ;; horizontal bingo
   (not (empty?  (filter (fn [line] (every? (fn [cell] (:hit cell))  line) )  board)))
   ;; vertical bingo
   (not (empty?  (filter (fn [line] (every? (fn [cell] (:hit cell))  line) )  (apply mapv vector board))))
   ;; x top-left to bottom-right
   ;; (loop [index 0]
   ;;   (if (> index (count board)) true
   ;;       (if (not (:hit (nth (nth board index) index)))
   ;;         false
   ;;         (recur (inc index)))))
   ;; x bottom-left to top-right
   ;; (loop [x-index (dec (count (first board)))
   ;;        y-index 0]
   ;;   (if (< x-index 0) true
   ;;       (if (not (:hit (nth (nth board y-index) x-index)))
   ;;         false
   ;;         (recur (dec x-index) (inc y-index)))))
   ))


(def test-board
  [[{:value 14, :hit false}
    {:value 21, :hit false}
    {:value 17, :hit true}
    {:value 24, :hit false}
    {:value 4, :hit true}]
   [{:value 10, :hit false}
    {:value 16, :hit false}
    {:value 15, :hit false}
    {:value 9, :hit true}
    {:value 19, :hit false}]
   [{:value 18, :hit false}
    {:value 8, :hit false}
    {:value 23, :hit true}
    {:value 26, :hit false}
    {:value 20, :hit false}]
   [{:value 22, :hit false}
    {:value 11, :hit true}
    {:value 13, :hit false}
    {:value 6, :hit false}
    {:value 5, :hit true}]
   [{:value 2, :hit true}
    {:value 0, :hit false}
    {:value 12, :hit false}
    {:value 3, :hit false}
    {:value 7, :hit true}]]
   )

(has-bingo? test-board)

(defn calculate-score [board n]
  (* n (reduce + (for [row board
                       c row
                       :when (not (:hit c))]
                   (:value c)))))

(defn part1
  ([] (part1 input))
  ([input]
   (let [bingo-numbers (->> input str/split-lines first (#(str/split % #","))  (map #(Integer/parseInt (str %))))
         boards (-> input (str/split #"\n\n") rest (#(mapv ->board %)))]
     (loop [numbers bingo-numbers
            boards boards]
       (let [ current (first numbers)
             new-boards (map (update-board current) boards)
             bingo! (first (filter has-bingo? new-boards))]
         (println current)
         (if bingo! (calculate-score bingo! current)
             (recur (rest numbers) new-boards)
             ))
       ))))

(comment
  (part1 example)
  (part1)
  )
