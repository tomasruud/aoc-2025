(use judge)

(def test-input ```
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
```)

(defn parse [input]
  (peg/match
    '{:main (some (* (group :line) (any "\n")))
      :line (some (+ (/ "." nil) (/ "@" :paper)))}
    input))

(defn is-paper-roll? [v]
  (= v :paper))

(defn lookup [grid x y]
  (get (get grid y []) x nil))

(defn count-adjacent-paper-rolls [grid x y]
  (->>
    [;(map |(lookup grid (+ x $) (- y 1)) [-1 0 1])
     ;(map |(lookup grid (+ x $) (+ y 0)) [-1 1])
     ;(map |(lookup grid (+ x $) (+ y 1)) [-1 0 1])]
    (count is-paper-roll?)))

(test (count-adjacent-paper-rolls (parse test-input) 2 0) 3)
(test (count-adjacent-paper-rolls (parse test-input) 1 1) 6)

(defn solve-1 [input]
  (def grid (parse input))
  (def h (length grid))
  (def w (length (first grid)))

  (->>
    (map
      (fn [i]
        (def x (% i w))
        (def y (math/floor (/ i h)))
        (and
          (is-paper-roll? (lookup grid x y))
          (< (count-adjacent-paper-rolls grid x y) 4)))
      (range (* h w)))
    (count true?)))

(test (solve-1 test-input) 13)

(defn solve-2 [input]
  (def grid (parse input))
  (def h (length grid))
  (def w (length (first grid)))

  (->>
    (map
      (fn [i]
        (def x (% i w))
        (def y (math/floor (/ i h)))
        (and
          (is-paper-roll? (lookup grid x y))
          (< (count-adjacent-paper-rolls grid x y) 4)))
      (range (* h w)))
    (count true?)))

(test (solve-2 test-input) 43)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $)})
    pp))
