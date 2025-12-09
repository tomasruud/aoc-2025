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
  (->>
    input
    (peg/match
      ~{:main (some (* :line :s*))
        :line (thru (group :roll))
        :roll (* "@" (column) (line))})
    # Normalize x and y coords to make testing more sensible.
    (map (fn [[x y]] [[(- x 2) (- y 1)] :paper]))
    from-pairs))

(test (has-key? (parse test-input) [2 0]) true)
(test (has-key? (parse test-input) [3 0]) true)
(test (has-key? (parse test-input) [4 0]) false)
(test (has-key? (parse test-input) [8 0]) true)
(test (has-key? (parse test-input) [9 0]) false)
(test (has-key? (parse test-input) [8 9]) true)
(test (has-key? (parse test-input) [9 9]) false)

(defn adjacent-rolls [rolls [x y]]
  (->>
    [;(map |[(+ x $) (- y 1)] (range -1 2 1))
     ;(map |[(+ x $) (+ y 0)] (range -1 2 2))
     ;(map |[(+ x $) (+ y 1)] (range -1 2 1))]
    (filter |(has-key? rolls $))))

(test (adjacent-rolls (parse test-input) [2 0]) @[[3 0] [1 1] [2 1]])
(test (adjacent-rolls (parse test-input) [1 1]) @[[2 0] [0 1] [2 1] [0 2] [1 2] [2 2]])

(defn is-accessible? [rolls pos]
  (< (length (adjacent-rolls rolls pos)) 4))

(defn solve-1 [input]
  (def rolls (parse input))
  (->>
    (keys rolls)
    (count |(is-accessible? rolls $))))

(test (solve-1 test-input) 13)

(defn remove-accessible [rolls]
  (def inaccessible
    (->>
      (pairs rolls)
      (filter |(not (is-accessible? rolls (first $))))
      from-pairs))

  (case (- (length rolls) (length inaccessible))
    0 inaccessible
    (remove-accessible inaccessible)))

(defn solve-2 [input]
  (def rolls (parse input))
  (def trimmed (remove-accessible rolls))
  (- (length rolls) (length trimmed)))

(test (solve-2 test-input) 43)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    pp))
