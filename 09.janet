(use judge)

(def test-input ```
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
```)

(defn parse [input]
  (peg/match
    '{:main (some (* (group :tile) (any "\n")))
      :tile (* :n "," :n)
      :n (number :d+)}
    input))

(defn area [[[ax ay] [bx by]]]
  (->>
    [(- ax bx) (- ay by)]
    (map math/abs)
    (map |(+ 1 $))
    (product)))

(test (area [[7 1] [11 7]]) 35)
(test (area [[11 7] [7 1]]) 35)
(test (area [[7 3] [2 3]]) 6)
(test (area [[2 5] [11 1]]) 50)

(defn diagonals [[a & points] &opt all]
  (default all @[])

  (each b points
    (array/push all [a b]))

  (if (empty? points)
    all
    (diagonals points all)))

(defn solve-1 [input]
  (->>
    (parse input)
    (diagonals)
    (map area)
    (max-of)))

(test (solve-1 test-input) 50)

(defn intersects [[[xa1 ya1] [xa2 ya2]] [[xb1 yb1] [xb2 yb2]]]
  (cond
    # Vertically parallel
    (and (= xa1 xa2) (= xb1 xb2)) (and (= xa1 xb1) (<= (min yb1 yb2) (min ya1 ya2) (max ya1 ya2) (max yb1 yb2)))
    # Horizontally parallel
    (and (= ya1 ya2) (= yb1 yb2)) (and (= ya1 yb1) (<= (min xb1 xb2) (min xa1 xa2) (max xa1 xa2) (max xb1 xb2)))

    (let [s ()
          t ()]
      (<= 0 (min s t) (max s t) 1))))

(defn box-for-diagonal [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) [[[x1 (min y1 y2)] [x1 (max y1 y2)]]]
    (= y1 y2) [[[(min x1 x2) y1] [(max x1 x2) y1]]]
    [[]
     []
     []
     []]))

(defn in-bounds [points]
  (def bounds @[])

  (eachk i points
    (array/push
      bounds
      [(i points) ((% (+ i 1) (length points)) points)]))

  (fn [diagonal]
    (->>
      (box-for-diagonal diagonal)
      (find
        (fn [line]
          (var crossings 0)

          (each bound bounds
            (when (intersects line bound) (++ crossings)))

          (even? crossings)))
      (nil?))))

(defn solve-2 [input]
  (def points (parse input))
  (->>
    (diagonals points)
    (filter (in-bounds points))
    (map area)
    (max-of)))

(test (solve-2 test-input) 24)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $)})
    (pp)))
