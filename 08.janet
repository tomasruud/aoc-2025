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
    sum
    math/sqrt))

(test (distance [1 2 3] [4 6 8]) 7.0710678118654755)

(defn all-distances [[point & points] &opt distances]
  (default distances (array/new 1_000_000))

  (each other points
    (array/push distances [(distance point other) [point other]]))

  (if (empty? points)
    distances
    (all-distances points distances)))

(defn connections-by-distance [points]
  (->>
    (all-distances points)
    sort
    (map 1)))

(test (slice (connections-by-distance (parse test-input)) 0 4)
      [[[162 817 812] [425 690 689]]
       [[162 817 812] [431 825 988]]
       [[906 360 560] [805 96 715]]
       [[431 825 988] [425 690 689]]])

(defn connect [num-connections circuits [[a b] & connections]]
  (var dex 0)
  (var next-circuits circuits)

  (def ia (find-index |(has-key? $ a) circuits))
  (def ib (find-index |(has-key? $ b) circuits))

  (unless (= ia ib)
    (def merged (merge (circuits ia) (circuits ib)))
    (put next-circuits ia merged)
    (set next-circuits (array/remove next-circuits ib))
    (++ dex))

  (if (= (- num-connections dex) 1)
    next-circuits
    (connect (- num-connections dex) next-circuits connections)))

(defn circuits-by-size [num-connections points]
  (->>
    (connections-by-distance points)
    (connect num-connections (map |@{$ :set} points))
    (map length)
    sort
    reverse))

(test (circuits-by-size 10 (parse test-input)) @[5 4 2 2 1 1 1 1 1 1 1])

(defn solve-1 [num-connections input]
  (->>
    (parse input)
    (circuits-by-size num-connections)
    (|(slice $ 0 3))
    product))

(test (solve-1 10 test-input) 40)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 1000 $)})))
