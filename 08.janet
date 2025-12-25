(use judge)

(def test-input ```
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
```)

(defn parse [input]
  (->>
    input
    (peg/match
      '{:main (some (group :box))
        :box (* :pos "," :pos "," :pos (any "\n"))
        :pos (number :d+)})
    (map slice)))

(defn distance [[ax ay az] [bx by bz]]
  (->>
    @[(- ax bx) (- ay by) (- az bz)]
    (map |(* $ $))
    (sum)
    (math/sqrt)))

(test (distance [1 2 3] [4 6 8]) 7.0710678118654755)

(defn connections-loop [[a & points] connections]
  (each b points
    (array/push connections [(distance a b) [a b]]))

  (if (empty? points)
    (map last (sort connections))
    (connections-loop points connections)))

(defn connections [points]
  (connections-loop
    points
    (array/new (math/pow (length points) 2))))

(test (take 4 (connections (parse test-input)))
      [[[162 817 812] [425 690 689]]
       [[162 817 812] [431 825 988]]
       [[906 360 560] [805 96 715]]
       [[431 825 988] [425 690 689]]])

(defn connect-loop [[[a b] & connections] circuits]
  (when-let [ia (find-index |(has-key? $ a) circuits)
             ib (find-index |(has-key? $ b) circuits)
             _ (not= ia ib)]
    (set (circuits ia) (merge (circuits ia) (circuits ib)))
    (set (circuits ib) nil))

  (if (empty? connections)
    (filter identity circuits)
    (connect-loop connections circuits)))

(defn connect [points connections]
  (connect-loop connections (map |@{$ :set} points)))

(defn circuits-by-size [n points]
  (->>
    (take n (connections points))
    (connect points)
    (map length)
    (sort-by -)))

(test (circuits-by-size 10 (parse test-input)) @[5 4 2 2 1 1 1 1 1 1 1])

(defn solve-1 [input &opt n]
  (default n 1000)
  (->>
    (parse input)
    (circuits-by-size n)
    (take 3)
    (product)))

(test (solve-1 test-input 10) 40)

(defn final-connection-loop [[[a b] & connections] circuit n]
  (set (circuit a) :set)
  (set (circuit b) :set)

  (if (= n (length circuit))
    [a b]
    (final-connection-loop connections circuit n)))

(defn final-connection [points]
  (final-connection-loop
    (connections points)
    (table/new (length points))
    (length points)))

(test (final-connection (parse test-input)) [[216 146 977] [117 168 530]])

(defn solve-2 [input]
  (->>
    (parse input)
    (final-connection)
    (map first)
    (flatten)
    (product)))

(test (solve-2 test-input) 25272)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    (pp)))
