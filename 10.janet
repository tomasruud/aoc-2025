(use judge)

(def test-input ```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
```)

(defn parse-config [lights buttons _]
  (def lights-as-decimal
    (reduce
      (fn [n [i state]]
        (pp [lights i state n])
        (bor n (blshift state n)))
      0
      (pairs (reverse lights))))

  [lights-as-decimal])

(defn parse [input]
  (peg/match
    ~{:main (some (* :line (any "\n")))

      :line (cmt :config ,parse-config)
      :config (* (group :lights) :s (group :buttons) :s (group :jolts))

      :lights (* "[" (some :light) "]")
      :light (+ (/ "." 0) (/ "#" 1))

      :buttons (* :button (any (* :s :button)))
      :button (group (* "(" :n-list ")"))

      :jolts (* "{" :n-list "}")

      :n-list (* :n (any (* "," :n)))
      :n (number :d+)}
    input))

(test (parse test-input)
      @[[6 [8 10 4 12 5]]
        [2 [29 12 17 7 30]]
        [29 [31 25 55 6]]])

(defn fewest-presses [config]
  0)

(test (fewest-presses (0 (parse test-input))) 2)
(test (fewest-presses (1 (parse test-input))) 3)
(test (fewest-presses (2 (parse test-input))) 2)

(defn solve-1 [input]
  (->>
    (parse input)
    (map fewest-presses)
    sum))

(test (solve-1 test-input) 7)
