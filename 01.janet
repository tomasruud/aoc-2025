(use judge)

(def test-input ```
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
```)

(def dial-size 100)
(def start-pos 50)

(defn dir [c]
  (case c
    "R" 1
    "L" -1))

(defn parse [input]
  (peg/match
    ~{:main (some (cmt :move ,*))
      :move (* :dir (number :d+) (any "\n"))
      :dir (cmt (<- (set "LR")) ,dir)}
    input))

(test (parse test-input)
      @[-68 -30 48 -5 60 -55 -1 -99 14 -82])

(defn solve-1 [input]
  (->>
    (parse input)
    (reduce
      (fn [[zeros pos] move]
        (def next-pos (mod (+ pos move) dial-size))
        (case next-pos
          0 [(+ zeros 1) next-pos]
          [zeros next-pos]))
      [0 start-pos])
    first))

(test (solve-1 test-input) 3)

(defn solve-2 [input]
  (->>
    (parse input)
    (reduce
      (fn [[zeros pos] move]
        (def full-rotations (div (math/abs move) dial-size))
        (def short-move (% move dial-size))

        (def dist (+ pos short-move))
        (def next-pos (mod dist dial-size))

        (def zero-clicks
          (cond
            (= pos 0) full-rotations
            (<= dist 0) (+ full-rotations 1)
            (>= dist dial-size) (+ full-rotations 1)
            full-rotations))

        [(+ zeros zero-clicks) next-pos])
      [0 start-pos])
    first))

(test (solve-2 test-input) 6)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    pp))
