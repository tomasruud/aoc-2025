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

(defn distance [[ax ay az] [bx by bz]]
  (->>
    @[(- ax bx) (- ay by) (- az bz)]
    (map |(* $ $))
    sum
    math/sqrt))

(test (distance [1 2 3] [4 6 8]) 7.0710678118654755)

(defn find-distances [[point & other-points] &opt distances]
  (default distances (array/new 1_000_000))

  (def next-distances
    (reduce
      (fn [acc other-point]
        (array/push acc [[point other-point] (distance point other-point)]))
      distances
      other-points))

  (if (empty? other-points)
    next-distances
    (find-distances other-points next-distances)))

(defn shortest-connections [points num-connections]
  (->>
    (find-distances points)
    (sorted-by last)
    (|(slice $ 0 (min num-connections (length $))))
    (map first)))

(test (shortest-connections (parse test-input) 4)
      @[[@[162 817 812] @[425 690 689]]
        [@[162 817 812] @[431 825 988]]
        [@[906 360 560] @[805 96 715]]
        [@[431 825 988] @[425 690 689]]])

(defn circuit-sizes [points num-connections]
  (->>
    (shortest-connections points num-connections)
    (map |(map slice $))
    (reduce
      (fn [circuits [a b]]
        (match [(find-index |(has-key? $ a) circuits)
                (find-index |(has-key? $ b) circuits)]
          [nil nil] (array/push circuits @{a :set b :set})
          [ia nil] (array/insert circuits ia (put (ia circuits) b :set))
          [nil ib] (array/insert circuits ib (put (ib circuits) a :set))
          [ia ib] (array/remove (array/insert circuits ia (merge (ia circuits) (ib circuits)) ib))))
      @[])
    pp
    (map length)
    sorted
    reverse))

(test (circuit-sizes (parse test-input) 10) @[4 3 2 2 1 1 1 1 1 1 1])

(defn solve-1 [input num-connections]
  (->>
    (parse input)
    (|(circuit-sizes $ num-connections))
    (|(slice $ 0 3))
    sum))

(test (solve-1 test-input 10) 40)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $ 1000)})))
