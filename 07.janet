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
    '{:main (some (* :line (any "\n")))
      :line (+ "." (group :sym))
      :sym (* (set "S^") (line) (column))}
    input))

(test (parse test-input)
      @[@[1 9]
        @[3 9]
        @[5 8] @[5 10]
        @[7 7] @[7 9] @[7 11]
        @[9 6] @[9 8] @[9 12]
        @[11 5] @[11 7] @[11 11] @[11 13]
        @[13 4] @[13 8] @[13 14]
        @[15 3] @[15 5] @[15 7] @[15 9] @[15 11] @[15 15]])

(defn splits [[[y x] & splitters] &opt init]
  (default init true)
  (if-let
    [next-split (find (fn [[my mx]] (and (= x mx) (< y my))) splitters)
     [ny nx] next-split
     next-splitters (filter (fn [[sy _]] (and (< ny sy))) splitters)
     left (splits [[ny (- nx 1)] ;next-splitters] false)
     right (splits [[ny (+ nx 1)] ;next-splitters] false)]
    (->>
      [(if init [] [y x]) left right]
      flatten
      (partition 2)
      (map |[$ :val])
      from-pairs
      keys
      sorted)
    []))

(test (splits (parse test-input))
      @[[3 8] [3 10]
        [5 7] [5 9] [5 11]
        [7 6] [7 8] [7 12]
        [9 5] [9 7] [9 9] [9 11] [9 13]
        [11 4] [11 8] [11 14]
        [13 3] [13 5] [13 7] [13 9] [13 15]])

(defn solve-1 [input]
  (->>
    (parse input)
    splits
    length))

(test (solve-1 test-input) 21)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $)})
    pp))
