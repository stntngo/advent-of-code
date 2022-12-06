(ns aoc.core-test
  (:require [clojure.test :as test]
            [clojure.string :as str]
            [aoc.core :as aoc]))

(defn read-str [s p]
  (mapv p (str/split-lines s)))

(def day-one-test-case "199
200
208
210
200
207
240
269
260
263")

(def day-two-test-case "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(def day-three-test-case "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(def day-four-test-case "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

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

(def day-five-test-case "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def day-six-test-case "3,4,3,1,2")

(def day-seven-test-case "16,1,2,0,4,2,7,1,2,14")

(def day-eight-test-case "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def day-nine-test-case "2199943210
3987894921
9856789892
8767896789
9899965678")

(def day-ten-test-case "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(test/deftest day-one-parse-test
  (test/testing "Day One Parse Test"
    (test/is (= 10 (count (read-str day-one-test-case #(Integer/parseInt %)))))))

(test/deftest day-one-depth-test
  (test/testing "Day One Window Test"
    (test/is (= 7 (aoc/sweep-floor
                   (read-str day-one-test-case #(Integer/parseInt %))
                   1)))))

(test/deftest day-one-window-test
  (test/testing "Day One Window Test"
    (test/is (= 5 (aoc/sweep-floor
                   (read-str day-one-test-case #(Integer/parseInt %))
                   3)))))

(test/deftest day-two-parse-test
  (test/testing "Day Two Parse Test"
    (test/is (= 6 (count
                   (read-str day-two-test-case aoc/parse-command))))))

(test/deftest day-two-aim-test
  (test/testing "Day Two Aim Test"
    (let [[x y aim] (reduce aoc/aim [0 0 0] (read-str day-two-test-case aoc/parse-command))]
      (test/is (= 150 (* x aim)))
      (test/is (= 900 (* x y))))))

(test/deftest day-three-power-test
  (test/testing "Day Three Power Test"
    (test/is (= 198 (-> day-three-test-case
                        (read-str aoc/parse-bits)
                        aoc/power-consumption)))))

(test/deftest day-three-life-support-test
  (test/testing "Day Three Life Support Test"
    (test/is (= 230 (-> day-three-test-case
                        (read-str aoc/parse-bits)
                        aoc/life-support)))))

(test/deftest day-four-parse-test
  (test/testing "Day Four Parse Test"
    (let [[random boards] (-> day-four-test-case
                              aoc/parse-bingo)
          [_ drawn _] (aoc/draw-number (random 0) (random 2))]
      (test/is (= 3 (count boards)))
      (test/is (= 7 drawn)))))

(test/deftest day-four-winners
  (test/testing "Day Four Winner Test"
    (let [[random boards] (-> day-four-test-case
                              aoc/parse-bingo)]
      (test/is (= 4512 (apply aoc/score-board (aoc/first-winner random boards))))
      (test/is (= 1924 (apply aoc/score-board (aoc/last-winner random boards)))))))

(test/deftest day-five-vents
  (test/testing "Day Five Test"
    (let [lines (-> day-five-test-case
                    (read-str aoc/parse-line))
          total (aoc/count-hot-spots lines)
          orthogonal (->> lines
                          (remove #(= :diagonal (apply aoc/line-type %)))
                          aoc/count-hot-spots)]
      (test/is (= 5 orthogonal))
      (test/is (= 12 total)))))

(test/deftest day-six-lanternfish
  (test/testing "Day Six Test"
    (let [population (aoc/parse-lantern-fish day-six-test-case)]
      (test/is (= 5934 (reduce + (aoc/simulate-population population 80))))
      (test/is (= 26984457539 (reduce + (aoc/simulate-population population 256)))))))

(test/deftest day-seven-optimization
  (test/testing "Day Seven Test"
    (let [nums (as-> day-seven-test-case t
                 (str/trim t)
                 (str/split t #",")
                 (map #(Integer/parseInt %) t))]
      (test/is (= 37 (aoc/abs-cost nums (aoc/median nums))))
      (test/is (= 168 (min
                       (aoc/arith-cost nums (int (Math/floor (aoc/mean nums))))
                       (aoc/arith-cost nums (int (Math/ceil (aoc/mean nums))))))))))

(test/deftest day-eight-search
  (test/testing "Day Eight Test"
    (let [signals (-> day-eight-test-case
                      (read-str aoc/parse-signal))]
      (test/is (= 26 (aoc/easy-digits signals)))
      (test/is (= 61229 (reduce + (map aoc/decode-output signals))))
      (test/is (= 61229 (reduce + (map aoc/decode-output-clever signals)))))))

(test/deftest day-nine-tests
  (test/testing "Day Nine Test"
    (let [lines (-> day-nine-test-case
                    (read-str aoc/parse-height-row))]
      (test/is (= 15 (aoc/risk-level lines)))
      (test/is (= 1134 (aoc/basin-factor lines))))))

(test/deftest day-ten-tests
  (test/testing "Day Ten Test"
    (let [[corrupt autocomplete] (as-> day-ten-test-case t
                                   (str/split-lines t)
                                   (map #(str/split % #"") t)
                                   (aoc/score-file t))]
      (test/is (= 26397 corrupt))
      (test/is (= 288957 autocomplete)))))

