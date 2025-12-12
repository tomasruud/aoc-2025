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

(defn find-splits [[initial-beams & splitters-lines]]
  (->>
    (reduce
      (fn [[current-beams splits] splitters]
        (def [next-beams next-splits]
          (reduce
            (fn [[beams splits] beam]
              (if-let [split (find |(= beam $) splitters)]
                [[;beams (- split 1) (+ split 1)] [;splits split]]
                [[;beams beam] splits]))
            [[] []]
            current-beams))
        [(->>
                               next-beams
                               (map |[$ :value])
                               from-pairs
                               keys
                               sorted) [;splits (->>
                               next-splits
                               (map |[$ :value])
                               from-pairs
                               keys
                               sorted)]])
      [initial-beams []]
      splitters-lines)
    last))

(test (find-splits (parse test-input))
      [@[9]
       @[8 10]
       @[7 9 11]
       @[6 8 12]
       @[5 7 11 13]
       @[4 8 14]
       @[3 5 7 9 15]])

(defn solve-1 [input]
  (->>
    input
    parse
    find-splits
    (map length)
    sum))

(test (solve-1 test-input) 21)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $)})
    pp))
