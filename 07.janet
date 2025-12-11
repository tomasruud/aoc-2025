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
    '{:main (some (* (+ :void :filled) (any "\n")))
      :void (some ".")
      :filled (group (some :splits))
      :splits (+ "." (* :sym (column)))
      :sym (set "S^")}
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

(defn splits [[start & splitters]]
  [])

(defn solve-1 [input]
  (->>
    (parse input)
    splits
    flatten
    length))

(test (solve-1 test-input) 21)
