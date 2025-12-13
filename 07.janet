(use judge)

(def test-input ```
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
```)

(defn parse [input]
  (peg/match
    '{:main (some (* (+ :void :fill) (any "\n")))
      :void (some ".")
      :fill (group (some (+ "." :sym)))
      :sym (* (set "S^") (column))}
    input))

(test (parse test-input)
      @[@[9]
        @[9]
        @[8 10]
        @[7 9 11]
        @[6 8 12]
        @[5 7 11 13]
        @[4 8 14]
        @[3 5 7 9 11 15]])

(defn unique [values]
  (reduce
    (fn [acc v]
      (if (= v (last acc))
        acc
        [;acc v]))
    []
    (sorted values)))

(test (unique [3 2 2 3 1 3 2 1]) [1 2 3])

(defn count-splits [[beams splitter & next-splitters] &opt splits]
  (default splits 0)

  (def [splitted-beams total-splits]
    (reduce
      (fn [[beams splits] beam]
        (if-let [split (find |(= beam $) splitter)]
          [[;beams (- split 1) (+ split 1)] (+ splits 1)]
          [[;beams beam] splits]))
      [[] splits]
      beams))

  (if (empty? next-splitters)
    total-splits
    (count-splits [(unique splitted-beams) ;next-splitters] total-splits)))

(defn solve-1 [input]
  (count-splits (parse input)))

(test (solve-1 test-input) 21)

(defn solve-2 [input]
  0)

(test (solve-2 test-input) 40)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $)})
    pp))
