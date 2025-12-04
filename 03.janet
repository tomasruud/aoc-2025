(use judge)

(def test-input ```
987654321111111
811111111111119
234234234234278
818181911112111
```)

(defn parse [input]
  (peg/match
    '{:main (some (* (group :bank) (any "\n")))
      :bank (some (number :d))}
    input))

(test (parse test-input)
      @[@[9 8 7 6 5 4 3 2 1 1 1 1 1 1 1]
        @[8 1 1 1 1 1 1 1 1 1 1 1 1 1 9]
        @[2 3 4 2 3 4 2 3 4 2 3 4 2 7 8]
        @[8 1 8 1 8 1 9 1 1 1 1 2 1 1 1]])

(defn largest-digit-at [ns]
  (reduce
    (fn [hi i]
      (if (> (i ns) (hi ns))
        i
        hi))
    0
    (range 1 (length ns))))

(test (largest-digit-at [1]) 0)
(test (largest-digit-at [1 1 2 9 1]) 3)

(defn max-joltage [bank &opt n]
  (default n 2)
  (->>
    (reduce
      (fn [[sum start] pos]
        (def end (+ (- (length bank) n) pos 1))
        (def i (+ start (largest-digit-at (slice bank start end))))
        (def mult (math/pow 10 (- n pos 1)))
        [(+ sum (* (i bank) mult)) (+ i 1)])
      [0 0]
      (range 0 n))
    first))

(test (max-joltage [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1]) 98)
(test (max-joltage [8 1 1 1 1 1 1 1 1 1 1 1 1 1 9]) 89)
(test (max-joltage [2 3 4 2 3 4 2 3 4 2 3 4 2 7 8]) 78)
(test (max-joltage [8 1 8 1 8 1 9 1 1 1 1 2 1 1 1]) 92)
(test (max-joltage [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1] 12) 987654321111)
(test (max-joltage [8 1 1 1 1 1 1 1 1 1 1 1 1 1 9] 12) 811111111119)
(test (max-joltage [2 3 4 2 3 4 2 3 4 2 3 4 2 7 8] 12) 434234234278)
(test (max-joltage [8 1 8 1 8 1 9 1 1 1 1 2 1 1 1] 12) 888911112111)

(defn solve-1 [input]
  (->>
    (parse input)
    (map max-joltage)
    sum))

(test (solve-1 test-input) 357)

(defn solve-2 [input]
  (->>
    (parse input)
    (map |(max-joltage $ 12))
    sum))

(test (solve-2 test-input) 3121910778619)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    pp))
