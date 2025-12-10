(use judge)

(def test-input ```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
```)

(defn parse [input]
  (peg/match
    '{:main (some (group :row))
      :row (* (some :cell) (any "\n"))
      :cell (* :space (+ :number :op) :space)
      :number (number :d+)
      :op (+ (/ "*" *) (/ "+" +))
      :space (any " ")}
    input))

(test (parse test-input)
      @[@[123 328 51 64]
        @[45 64 387 23]
        @[6 98 215 314]
        @[* + * +]])

(defn rotate90 [matrix]
  (reduce
    (fn [cols row]
      (map
        |[(get row $) ;(get cols $ [])]
        (range 0 (max (length row) (length cols)))))
    []
    matrix))

(test (rotate90 (parse test-input))
      @[[* 6 45 123]
        [+ 98 64 328]
        [* 215 387 51]
        [+ 314 23 64]])

(defn solve-1 [input]
  (->>
    (parse input)
    rotate90
    (map eval)
    sum))

(test (solve-1 test-input) 4277556)

(defn parse-2 [input]
  (defn parse
    "Parse individual tokens and group them by line"
    [input]
    (peg/match
      '{:main (some (* (group :line) (any "\n")))
        :line (some (+ (/ " " nil) (<- (+ :d "*" "+"))))}
      input))

  (defn stringify
    "Stringifies tokens to one string"
    [tokens]
    (->>
      (map |(filter string? $) tokens)
      (map string/join)
      (|(string/join $ " "))))

  (defn tokenize
    "Tokenizes string and groups expressions"
    [str]
    (->>
      str
      (peg/match
        ~{:main (some (group :exp))
          :exp (* :op :space (some (* :number :space)))
          :number (cmt (cmt (<- :d+) ,reverse) ,scan-number)
          :op (+ (/ "*" *) (/ "+" +))
          :space (any " ")})
      (map slice)))

  (->>
    input
    parse
    rotate90
    stringify
    tokenize))

(test (parse-2 test-input)
      @[[* 1 24 356]
        [+ 369 248 8]
        [* 32 581 175]
        [+ 623 431 4]])

(defn solve-2 [input]
  (->>
    (parse-2 input)
    (map eval)
    sum))

(test (solve-2 test-input) 3263827)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    pp))
