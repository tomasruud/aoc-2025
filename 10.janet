(use judge)

(def test-input ```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
```)

(defn translate-config [lights buttons _]
  (def lights-decimal
    (reduce
      (fn [acc [i state]]
        (bor acc (blshift state i)))
      0
      (pairs lights)))

  (def buttons-decimal
    (map
      |(reduce
         (fn [acc i]
           (bor acc (blshift 1 i)))
         0 $)
      buttons))

  [lights-decimal buttons-decimal])

(defn parse [input]
  (peg/match
    ~{:main (some (* :line (any "\n")))

      :line (cmt :config ,translate-config)
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
      @[[6 @[8 10 4 12 5 3]]
        [8 @[29 12 17 7 30]]
        [46 @[31 25 55 6]]])

(defn fewest-indicator-presses [[target buttons _] &opt n prev-combos]
  (default n 1)
  (default prev-combos @{0 :set})

  (def next-combos
    (table/new (* (length prev-combos) (length buttons))))

  (eachk prev prev-combos
    (each button buttons
      (put next-combos (bxor prev button) :set)))

  (if (has-key? next-combos target)
    n
    (fewest-indicator-presses [target buttons] (inc n) next-combos)))

(test (fewest-indicator-presses (0 (parse test-input))) 2)
(test (fewest-indicator-presses (1 (parse test-input))) 3)
(test (fewest-indicator-presses (2 (parse test-input))) 2)

(defn solve-1 [input]
  (->>
    (parse input)
    (map fewest-indicator-presses)
    sum))

(test (solve-1 test-input) 7)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $)})
    (pp)))
