(ns aoc.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.set :refer [intersection]]
            [clojure.core.match :refer [match]]
            [clojure.algo.generic.functor :refer [fmap]]))

(defn read-file [p f]
  (with-open [rdr (io/reader f)]
    (->> rdr
         line-seq
         (mapv p))))

(defn input [day]
  (str "../go/input/day-" (format "%02d" day)))

; Day One
(defn sweep-floor [readings window]
  (let [floor (->> readings
                   (partition window 1)
                   (map #(reduce + %)))]

    (->> (map - floor (next floor))
         (filter #(> 0 %))
         count)))

(defn day-one []
  (let [sweep (->> (input 1)
                   (read-file #(Integer/parseInt %))
                   (partial sweep-floor))]
    (println "Day One")
    (println "Part One:" (sweep 1))
    (println "Part Two:" (sweep 3))))

; Day Two
(defn parse-command [line]
  (let [[dir dist] (str/split line #" ")]
    (vector (case dir
              "up" :up
              "down" :down
              "forward" :forward)
            (Integer/parseInt dist))))

(defn aim [[x y aim] [direction distance]]
  (case direction
    :forward [(+ x distance) (+ y (* aim distance)) aim]
    :up [x y (- aim distance)]
    :down [x y (+ aim distance)]))

(defn day-two []
  (let [[x y aim] (->> (input 2)
                       (read-file parse-command)
                       (reduce aim [0 0 0]))]
    (println "Day Two")
    (println "Part One:" (* x aim))
    (println "Part Two:" (* x y))))

; Day Three
(defn parse-bit-array [bits]
  (Integer/parseInt (str/join bits) 2))

(defn parse-bits [line]
  (str/split line #""))

(defn transpose [matrix]
  (apply map vector matrix))

(defn gamma-bit [{zero "0" one "1"}]
  (if (> one zero) "1" "0"))

(defn flip [bit]
  (case bit
    "0" "1"
    "1" "0"))

(defn power-consumption [numbers]
  (let [gamma (->> numbers
                   transpose
                   (map #(gamma-bit (frequencies %))))
        epsilon (map flip gamma)]
    (* (parse-bit-array gamma) (parse-bit-array epsilon))))

(defn bit-filter [numbers idx f]
  (if (= 1 (count numbers))
    (first numbers)
    (let [counter (->> numbers
                       transpose
                       (map frequencies))
          target (-> counter
                     (nth idx)
                     f)]
      (-> (filter (fn [line] (-> line
                                 (nth idx)
                                 (= target)))
                  numbers)
          (recur (inc idx) f)))))

(defn life-support [numbers]
  (let [carbon (-> numbers
                   (bit-filter 0 (fn [{zero "0" one "1"}] (if (>= one zero) "1" "0")))
                   parse-bit-array)
        oxygen (-> numbers
                   (bit-filter 0 (fn [{zero "0" one "1"}] (if (<= zero one) "0" "1")))
                   parse-bit-array)]
    (* carbon oxygen)))

; Day Three Trie Version
(defrecord Node [count zero one])

(defn weighted-trie [[head & tail] node]
  (let [node (if node
               (update node :count inc)
               (Node. 1 nil nil))]
    (case head
      "0" (update node :zero #(weighted-trie tail %))
      "1" (update node :one #(weighted-trie tail %))
      nil node)))

(defn trie-filter [node f]
  (when (and node (or (:zero node) (:one node)))
    (let [[val next] (f (:zero node) (:one node))]
      (conj (trie-filter next f) val))))

(defn oxygen-trie-filter [zero one]
  (cond
    (and zero one) (if (>= (:count one) (:count zero))
                     [1 one]
                     [0 zero])
    zero [0 zero]
    one [1 one]
    :else [nil nil]))

(defn carbon-trie-filter [zero one]
  (cond
    (and zero one) (if (<= (:count zero) (:count one))
                     [0 zero]
                     [1 one])
    zero [0 zero]
    one [1 one]
    :else [nil nil]))

(defn life-support-trie [numbers]
  (let [root (->> numbers
                  (reduce #(weighted-trie %2 %1) (Node. 0 nil nil)))
        carbon (-> root
                   (trie-filter carbon-trie-filter)
                   parse-bit-array)
        oxygen (-> root
                   (trie-filter oxygen-trie-filter)
                   parse-bit-array)]
    (* carbon oxygen)))

(defn day-three []
  (let [numbers (->> (input 3)
                     (read-file parse-bits))
        power (power-consumption numbers)
        life (life-support numbers)
        life-trie (life-support-trie numbers)]

    (println "Day Three")
    (println "Part One:" power)
    (println "Part Two:" life)
    (println "Part Two (Trie):" life-trie)))

; Day Four
(defn parse-board [board]
  (->> board
       (map (fn [line]
              (as-> line v
                (str/split v #" ")
                (remove str/blank? v)
                (map #(Integer/parseInt %) v))))))

(defn parse-bingo [input]
  (let [[raw-numbers & raw-boards] (->> input
                                        str/split-lines
                                        (remove str/blank?))
        random (as-> raw-numbers nums
                 (str/split nums #",")
                 (map #(Integer/parseInt %) nums))
        boards  (->> raw-boards
                     (partition 5)
                     (map parse-board))]

    [[random nil #{}] boards]))

(defn draw-number [[pick & numbers] drawn]
  [numbers pick (conj drawn pick)])

(defn winner? [drawn board]
  (or (->> board
           (some #(every? drawn %)))
      (->> board
           transpose
           (some #(every? drawn %)))))

(defn score-board [last-pick drawn board]
  (->> board
       flatten
       (remove drawn)
       (reduce +)
       (* last-pick)))

(defn first-winner [[numbers last-pick drawn] boards]
  (if-let [winner (->> boards
                       (filter #(winner? drawn %))
                       first)]
    [last-pick drawn winner]
    (recur (draw-number numbers drawn) boards)))

(defn last-winner [[numbers last-pick drawn] boards]
  (if (every? #(winner? drawn %) boards)
    [last-pick drawn (first boards)]
    (recur
     (draw-number numbers drawn)
     (remove #(winner? drawn %) boards))))

(defn day-four []
  (let [[random boards] (->> (input 4)
                             slurp
                             parse-bingo)]
    (println "Day Four")
    (println "Part One:" (apply score-board (first-winner random boards)))
    (println "Part Two:" (apply score-board (last-winner random boards)))))

; Day Five
(defrecord Point [x y])

(defn parse-point [s]
  (as-> s s
    (str/split s #",")
    (mapv #(Integer/parseInt %) s)
    (apply ->Point s)))

(defn parse-line [s]
  (as-> s s
    (str/split s #" -> ")
    (map parse-point s)))

(defn line-type [{x :x y :y}
                 {x' :x y' :y}]
  (cond
    (= x x') :vertical
    (= y y') :horizontal
    :else :diagonal))

(defn xrange [x y]
  (if (> x y)
    (reverse (range y (inc x)))
    (range x (inc y))))

(defn line-points [{x :x y :y}
                   {x' :x y' :y}]
  (case (line-type (Point. x y) (Point. x' y'))
    :vertical (map #(->Point x %) (xrange y y'))
    :horizontal (map #(->Point % y) (xrange x x'))
    :diagonal (map #(->Point %1 %2) (xrange x x') (xrange y y'))))

(defn count-hot-spots [lines]
  (->> lines
       (map #(apply line-points %))
       flatten
       frequencies
       seq
       (remove (fn [[_ n]] (= 1 n)))
       count))

(defn day-five []
  (let [lines (->> (input 5)
                   (read-file parse-line))
        total (count-hot-spots lines)
        no-diag (->> lines
                     (remove #(= :diagonal (apply line-type %)))
                     count-hot-spots)]
    (println "Day Five")
    (println "Part One:" no-diag)
    (println "Part Two:" total)))

; Day Six
(defn parse-lantern-fish [s]
  (->> (str/split s #",")
       (map #(Integer/parseInt %))
       (reduce
        (fn [acc idx]
          (update acc idx inc))
        (apply vector (repeat 9 0)))))

(defn simulate-population [population days]
  (if (= 0 days)
    population
    (let [[head & tail] population
          population (conj (apply vector tail) head)]
      (recur
       (update population 6 #(+ head %))
       (dec days)))))

(defn day-six []
  (let [population (-> (input 6)
                       slurp
                       parse-lantern-fish)
        sim (partial simulate-population population)]
    (println "Day Six")
    (println "Part One:" (reduce + (sim 80)))
    (println "Part Two:" (reduce + (sim 256)))))

; Day Seven
(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (/ sum count)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn abs-cost [nums x]
  (reduce + (map #(Math/abs (- % x)) nums)))

(defn arithmetic-sum [x]
  (/ (* x (inc x)) 2))

(defn arith-cost [nums x]
  (reduce + (map  #(arithmetic-sum (Math/abs (- % x))) nums)))

(defn day-seven []
  (let [nums (as-> (input 7) i
               (slurp i)
               (str/trim i)
               (str/split i #",")
               (map #(Integer/parseInt %) i))]
    (println "Day Seven")
    ; The value that minimizes (reduce + (map #(Math/abs (- % x)) s)) for a given
    ; set s is the median of that set. The L1 norm.
    (println "Part One:" (abs-cost nums (median nums)))
    ; In turn the value that minimizes the sum of the arithmetic sequences of the difference
    ; between each element of that set and nil is an integer +/- the mean of the set s.
    (println "Part Two:" (min
                          (arith-cost nums (int (Math/floor (mean nums))))
                          (arith-cost nums (int (Math/ceil (mean nums))))))))

; Day Eight
(defn parse-signal [line]
  (as-> line l
    (str/split l #" \| ")
    (map #(str/split % #" ") l)))

(defn easy-digits [signals]
  (->> signals
       (map second)
       flatten
       (filter #(#{2 3 4 7} (count %)))
       count))

(def ^:private digits [#{0 1 2 4 5 6},   ; 0
                       #{2 5},           ; 1
                       #{0 2 3 4 6},     ; 2
                       #{0 2 3 5 6},     ; 3
                       #{1 2 3 5},       ; 4
                       #{0 1 3 5 6},     ; 5
                       #{0 1 3 4 5 6},   ; 6
                       #{0 2 5},         ; 7
                       #{0 1 2 3 4 5 6}, ; 8
                       #{0 1 2 3 5 6},   ; 9
                       ])

(def ^:private permutations
  (apply vector (combo/permutations "abcdefg")))

(defn resolve-digit [digit candidate]
  (->> digit
       (map #(nth candidate %))
       set))

(defn wiring [candidate]
  (map #(resolve-digit % candidate) digits))

(defn matching-digit? [candidate digit]
  (and (= (count candidate) (count digit))
       (every? candidate digit)))

(defn valid-wiring? [[candidate & wiring] signal]
  (if (nil? candidate)
    true
    (if-let [match (->> signal
                        (filter #(matching-digit? candidate %))
                        first)]
      (recur wiring (remove #(= match %) signal))
      false)))

(defn decode-digit [decoded digit]
  (first (keep-indexed #(when (matching-digit? %2 digit) %1) decoded)))

(defn decode-signal [signal]
  (->> permutations
       (map wiring)
       (filter #(valid-wiring? % signal))
       first))

(defn decode-output [[signal output]]
  (let [decoded (decode-signal signal)]
    (->> output
         (map #(decode-digit decoded %))
         (map-indexed #(* (math/expt 10 (- 3 %1)) %2))
         (reduce +))))

(defn decode-digit-clever [masks digit]
  (let [digit (set digit)
        four (get masks 4)
        one (get masks 2)]
    (match [(count digit)
            (count (intersection digit four))
            (count (intersection digit one))]
      [2 _ _] 1
      [3 _ _] 7
      [4 _ _] 4
      [7 _ _] 8
      [5 2 _] 2
      [5 3 1] 5
      [5 3 2] 3
      [6 4 _] 9
      [6 3 1] 6
      [6 3 2] 0)))

(defn decode-output-clever [[signal output]]
  (let [masks (->> signal
                   (reduce
                    (fn [acc x]
                      (assoc acc (count x) (set x)))
                    {}))]
    (->> output
         (map #(decode-digit-clever masks %))
         (apply str)
         Integer/parseInt)))

(defn day-eight []
  (let [signals (->> (input 8)
                     (read-file parse-signal))
        easy-count (easy-digits signals)
        output-sum (->> signals
                        (map decode-output-clever)
                        (reduce +))]
    (println "Day Eight")
    (println "Part One:" easy-count)
    (println "Part Two:" output-sum)))

; Day Nine
(defn row-lows [line]
  (let [parts (partition 3 1 line)
        [h1 h2 & _] (first parts)
        [t1 t2 & _] (reverse (last parts))]
    (flatten [(< h1 h2)
              (map (fn [[x y z]] (and (< y x) (< y z))) parts)
              (< t1 t2)])))

(defn low-points [lines]
  (let [row-wise (flatten (map row-lows lines))
        col-wise (flatten (transpose
                           (map row-lows (transpose lines))))]
    (mapv #(and %1 %2)
          row-wise
          col-wise)))

(defn risk-level [lines]
  (let [low-key (low-points lines)]
    (->> lines
         flatten
         (keep-indexed #(when (get low-key %1) %2))
         (map inc)
         (reduce +))))

(defn neighbors [x y]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]])

(defn map-basin
  ([coord edges]
   (map-basin coord edges #{}))
  ([coord edges basin]
   (let [basin (conj basin coord)]
     (reduce #(map-basin %2 edges %1) basin  (edges coord)))))

(defn connections [lines]
  (let [line (first lines)]
    (fn [[x y]]
      (let [value (get (get lines y) x)]
        (->> [x y]
             (apply neighbors)
             (filterv (fn [[x y]]
                        (let [nval (get (get lines y) x)]
                          (and
                           (and (>= x 0) (< x (count line)))
                           (and (>= y 0) (< y (count lines)))
                           (< nval 9)
                           (> nval value))))))))))

(defn basin-factor [lines]
  (let [line (first lines)
        coords (->> (combo/cartesian-product
                     (range (count lines))
                     (range (count line)))
                    (map #(apply vector (reverse %))))
        low-key (low-points lines)
        minima (->> coords
                    (keep-indexed #(when (get low-key %1) %2)))
        edges (->> coords
                   (mapv (connections lines))
                   (interleave coords)
                   (apply hash-map))]
    (->> minima
         (map #(map-basin % edges))
         (map count)
         (sort >)
         (take 3)
         (reduce *))))

(defn parse-height-row [s]
  (->> (str/split s #"")
       (mapv #(Integer/parseInt %))))

(defn day-nine []
  (let [lines (->> (input 9)
                   (read-file parse-height-row))]
    (println "Day Nine")
    (println "Part One:" (risk-level lines))
    (println "Part Two:" (basin-factor lines))))

; Day Ten
(defn validate-syntax
  ([body]
   (validate-syntax body '()))
  ([[head & body] stack]
   (if head
     (case head
       "(" (validate-syntax body (conj stack ")"))
       "[" (validate-syntax body (conj stack "]"))
       "{" (validate-syntax body (conj stack "}"))
       "<" (validate-syntax body (conj stack ">"))
       (let [[sym & stack] stack]
         (if (= head sym)
           (recur body stack)
           [head nil])))
     [nil stack])))

(defn score-stack [stack]
  (reduce (fn [acc sym]
            (let [sym-value (case sym
                              ")" 1
                              "]" 2
                              "}" 3
                              ">" 4)]
              (-> acc
                  (* 5)
                  (+ sym-value)))) 0 stack))

(defn score-file [lines]
  (let [validated (as-> lines l
                    (map validate-syntax l)
                    (transpose l))
        corrupt (->> validated
                     first
                     (remove nil?)
                     (map (fn [sym]
                            (case sym
                              ")" 3
                              "]" 57
                              "}" 1197
                              ">" 25137)))
                     (reduce +))
        stack (->> validated
                   second
                   (remove nil?)
                   (map score-stack)
                   median)]

    [corrupt stack]))

(defn day-ten []
  (let [[corrupt autocomplete] (as-> (input 10) l
                                 (slurp l)
                                 (str/split-lines l)
                                 (map #(str/split % #"") l)
                                 (score-file l))]
    (println "Day Ten")
    (println "Part One:" corrupt)
    (println "Part Two:" autocomplete)))

; Day Eleven
(def cavern-size 10)

(defn parse-cavern [grid]
  (->> grid
       (map-indexed (fn [y row]
                      (map-indexed (fn
                                     [x val] [(->Point x y) val]) row)))
       flatten
       (apply hash-map)))

(defn adjacent [point]
  (let [{x :x y :y} point
        adj (range -1 2)]
    (->> adj
         (map (fn [dx]
                (map #(->Point (+ dx x) (+ % y)) adj)))
         flatten
         (remove (fn [{x' :x y' :y}]
                   (or
                    (< x' 0)
                    (< y' 0)
                    (>= x' cavern-size)
                    (>= y' cavern-size)
                    (and
                     (= x x')
                     (= y y'))))))))

(defn step [cavern]
  (let [cavern (fmap inc cavern)
        round-flash (->> cavern
                         seq
                         (filter (fn [[_ energy]] (>= energy 9)))
                         (map #(get % 0)))
        flashed (set round-flash)]
    ((fn [cavern round-flash flashed]
       (if (= 0 (count round-flash))
         [cavern flashed]
         (let [cavern (->> round-flash
                           (map adjacent)
                           flatten
                           (reduce #(update %1 %2 inc) cavern))
               round-flash (->> cavern
                                seq
                                (filter (fn [[_ energy]] (>= energy 9)))
                                (map #(get % 0))
                                (remove flashed))
               flashed (->> round-flash
                            (reduce #(conj %1 %2) flashed))]
           (recur cavern round-flash flashed))))
     cavern
     round-flash
     flashed)))

; Day Twelve
; Day Thirteen
(defn parse-fold [s]
  (let [[_ axis value] (re-matches
                        #"fold along (\w)=(\d+)"
                        s)
        axis (case axis
               "x" :x
               "y" :y)
        value (Integer/parseInt value)]
    [axis value]))

(defn parse-lite-brite []
  (let [[coords _ folds] (->> (input 13)
                              slurp
                              (str/split-lines)
                              (partition-by empty?)
                              (apply vector))
        dots (->> coords
                  (map (fn [coord]
                         (as-> coord c
                           (str/split c #",")
                           (map #(Integer/parseInt %) c))))
                  set)
        instructions (->> folds
                          (mapv parse-fold))]

    [dots instructions]))

(defn abs [x]
  (max x (- x)))

(defn fold-dot [[x y] [dir value]]
  (case dir
    :y [x (- value  (abs (- value y)))]
    :x [(- value (abs (- value x))) y]))

(defn fold-dots [dots [inst & remaining]]
  (if inst
    (recur (set (map #(fold-dot % inst) dots)) remaining)
    dots))

(defn print-dots [dots]
  (let [mx (apply max (map first dots))
        my (apply max (map second dots))]
    (loop [y 0]
      (when (<= y my)
        (->> mx
             inc
             range
             (map #(if (dots [% y]) "#" " "))
             (apply str)
             (println))
        (recur (inc y))))))

(defn day-thirteen []
  (let [[dots instructions] (parse-lite-brite)
        final (fold-dots dots instructions)]
    (println "Day Thirteen")
    (println "Part One:" (count (fold-dots dots (subvec instructions 0 1))))
    (println "Part Two:")
    (print-dots final)))

; Day Fourteen
(defn parse-rule [rule]
  (let [[[a b] insertion] (str/split rule #" -> ")]
    [(str a b) [(str a insertion) (str insertion b)]]))

(defn parse-polymers [s]
  (let [[template & rules] (->> s
                                str/split-lines
                                (remove empty?))
        init (first template)
        rules (->> rules
                   (map parse-rule)
                   (apply concat)
                   (apply hash-map))
        template (->> template
                      (partition 2 1)
                      (map #(apply str %))
                      frequencies)]
    [template init rules]))

(defn weighted-frequencies [coll]
  (->> coll
       (reduce (fn [acc [k cnt]]
                 (assoc acc k
                        (+ (get acc k 0) cnt))) {})))

(defn apply-rules [polymers rules]
  (->> polymers
       (map (fn [[pair count]]
              (map #(vector % count) (rules pair))))
       (apply concat)
       weighted-frequencies))

(defn score-polymer [init coll]
  (let [counts (as-> coll c
                 (map (fn [[[_ char] count]]
                        [char count]) c)
                 (weighted-frequencies c)
                 (update c init inc)
                 (vals c))
        max (apply max counts)
        min (apply min counts)]
    (- max min)))

(defn day-fourteen [n]
  (let [[template init rules] (->> (input 14)
                                   slurp
                                   parse-polymers)]
    (score-polymer init (loop [template template
                               i 0]
                          (if (< i n)
                            (recur (apply-rules template rules) (inc i))
                            template)))))

; Day Fifteen
; Day Sixteen
; Day Seventeen
; Day Eighteen
; Day Nineteen
; Day Twenty
; Day Twenty-One
; Day Twenty-Two
; Day Twenty-Three
; Day Twenty-Four
; Day Twenty-Five

(defn -main []
  (day-one)
  (println "")
  (day-two)
  (println "")
  (day-three)
  (println "")
  (day-four)
  (println "")
  (day-five)
  (println "")
  (day-six)
  (println "")
  (day-seven)
  (println "")
  (day-eight)
  (println "")
  (day-nine)
  (println "")
  (day-ten)
  (println "")
  (day-thirteen))









