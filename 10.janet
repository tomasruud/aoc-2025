(use judge)

(def test-input ```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
```)

(defn parse [config-parser input]
  (peg/match
    ~{:main (some (* :line (any "\n")))

      :line (cmt :config ,config-parser)
      :config (* (group :lights) :s (group :buttons) :s (group :jolts))

      :lights (* "[" (some :light) "]")
      :light (+ (/ "." 0) (/ "#" 1))

      :buttons (* :button (any (* :s :button)))
      :button (group (* "(" :n-list ")"))

      :jolts (* "{" :n-list "}")

      :n-list (* :n (any (* "," :n)))
      :n (number :d+)}
    input))

(defn indicator-config [lights buttons _]
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

(test (parse indicator-config test-input)
      @[[6 @[8 10 4 12 5 3]]
        [8 @[29 12 17 7 30]]
        [46 @[31 25 55 6]]])

(defn fewest-indicator-presses [[target buttons] &opt n prev-combos]
  (default n 1)
  (default prev-combos @{0 :set})

  (def next-combos @{})

  (eachk prev prev-combos
    (each button buttons
      (put next-combos (bxor prev button) :set)))

  (if (has-key? next-combos target)
    n
    (fewest-indicator-presses [target buttons] (inc n) next-combos)))

(test (fewest-indicator-presses [6 @[8 10 4 12 5 3]]) 2)
(test (fewest-indicator-presses [8 @[29 12 17 7 30]]) 3)
(test (fewest-indicator-presses [46 @[31 25 55 6]]) 2)

(defn solve-1 [input]
  (->>
    (parse indicator-config input)
    (map fewest-indicator-presses)
    sum))

(test (solve-1 test-input) 7)

(defn joltage-config [_ buttons jolts]
  (def jolts-sorted
    (->>
      (pairs jolts)
      (map reverse)
      (map slice)
      (sorted)))

  (def mapping
    (->>
      (map last jolts-sorted)
      (pairs)
      (reduce
        (fn [acc [new-idx old-idx]]
          (put acc old-idx new-idx))
        @{})))

  (def next-buttons
    (->>
      buttons
      (map |(map |($ mapping) $))
      (map slice)
      (slice)))

  (def next-jolts
    (->>
      jolts-sorted
      (map first)
      (slice)))

  [next-jolts next-buttons])

(test (parse joltage-config test-input)
      @[[[3 4 5 7] [[3] [2 3] [1] [1 3] [0 1] [0 2]]]
        [[2 5 7 7 12] [[2 4 3 0] [4 3] [2 0] [2 1 4] [1 4 3 0]]]
        [[5 5 10 10 11 11] [[2 4 5 0 3] [2 0 3] [2 4 5 3 1] [4 5]]]])

(defn fewest-joltage-presses [[target buttons] &opt n states]
  (default n 1)
  (default states @{(slice (array/new-filled (length target) 0)) :set})

  (def next-states @{})

  (eachk state states
    (each button buttons
      (def new-state (thaw state))

      (each press button
        (++ (new-state press)))

      (when-let [s (slice new-state)
                 _ (<= s target)]
        (put next-states s :set))))

  (if (has-key? next-states target)
    n
    (fewest-joltage-presses [target buttons] (inc n) next-states)))

(defn solve-2 [input]
  (->>
    (parse joltage-config input)
    (map fewest-joltage-presses)
    sum))

(test (solve-2 test-input) 33)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    (pp)))
