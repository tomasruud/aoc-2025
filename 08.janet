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
  (peg/match
    '{:main (some (group :box))
      :box (* :pos "," :pos "," :pos (any "\n"))
      :pos (number :d+)}
    input))

(defn euclidean-distance [[ax ay az] [bx by bz]]
  (->>
    [(- ax bx) (- ay by) (- az bz)]
    (map |(math/pow $ 2))
    sum
    math/sqrt))

(test (euclidean-distance [1 2 3] [4 6 8]) 7.0710678118654755)

(defn find-closest [idx junctions]
  (def junction (idx junctions))
  (->>
    (pairs junctions)
    (reduce
      (fn [[min-i min-dist] [i other]]
        (def dist (euclidean-distance junction other))
        (cond
          (= junction other) [min-i min-dist]
          (nil? min-i) [i dist]
          (< dist min-dist) [i dist]
          [min-i min-dist]))
      [nil nil])
    first))

(test (find-closest 0 (parse test-input)) 19)

(defn find-circuits [junctions]
  (->>
    junctions
    pairs
    (reduce
      (fn [circuits [idx junction]]
        circuits)
      [])
    (map keys)))

(test (find-circuits (parse test-input)) nil)

(defn solve-1 [input]
  (->>
    (parse input)
    find-circuits
    (map length)
    sorted
    reverse
    (slice 0 3)
    *))

(test (solve-1 test-input) 40)
