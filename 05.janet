(use judge)

(def test-input ```
3-5
10-14
16-20
12-18

1
5
8
11
17
32
```)

(defn parse [input]
  (peg/match
    ~{:main (* (group :fresh-ids) :s (group :ingredient-ids))
      :fresh-ids (some (group (* :id "-" :id "\n")))
      :ingredient-ids (some (* :id (any "\n")))
      :id (number :d+)}
    input))

(test (parse test-input)
      @[@[@[3 5] @[10 14] @[16 20] @[12 18]]
        @[1 5 8 11 17 32]])

(defn is-fresh [ranges id]
  (truthy?
    (find
      (fn [[lo hi]] (<= lo id hi))
      ranges)))

(test (is-fresh (first (parse test-input)) 1) false)
(test (is-fresh (first (parse test-input)) 5) true)
(test (is-fresh (first (parse test-input)) 8) false)
(test (is-fresh (first (parse test-input)) 11) true)
(test (is-fresh (first (parse test-input)) 17) true)
(test (is-fresh (first (parse test-input)) 32) false)

(defn solve-1 [input]
  (def [ranges ids] (parse input))
  (count |(is-fresh ranges $) ids))

(test (solve-1 test-input) 3)

(defn merge-overlapping [ranges]
  (def sorted (sorted-by first ranges))
  (reduce
    (fn [merged [clo chi]]
      (def [plo phi] (last merged))
      (if (<= plo clo phi)
        [;(slice merged 0 -2) [plo (max chi phi)]]
        [;merged [clo chi]]))
    (slice sorted 0 1)
    (slice sorted 1)))

(test (merge-overlapping (first (parse test-input))) [@[3 5] [10 20]])

(defn solve-2 [input]
  (->>
    (first (parse input))
    merge-overlapping
    (map |(- ;(reverse $) -1))
    sum))

(test (solve-2 test-input) 14)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    pp))
