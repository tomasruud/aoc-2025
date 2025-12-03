(use judge)

(def test-input ```
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
```)

(defn id-range
  "Generates a full range of string IDs, with hi inclusive"
  [lo hi]
  (range lo (+ 1 hi)))

(test (id-range 1 5) @[1 2 3 4 5])

(defn parse [input]
  (peg/match
    ~{:main (some (* (cmt :range ,id-range) (any ",") (any "\n")))
      :range (* (number :d+) "-" (number :d+))}
    input))

(test (get (parse test-input) 10)
      @[2121212118
        2121212119
        2121212120
        2121212121
        2121212122
        2121212123
        2121212124])

(defn digits [n]
  (def x (math/abs n))
  (if (= n 0)
    1
    (+ 1 (math/floor (math/log10 x)))))

(test (digits 10) 2)
(test (digits 1012) 4)

(defn digit-slice [n start end]
  (math/floor (% (/ n (math/pow 10 start)) (math/pow 10 (- end start)))))

(test (digit-slice 12345 1 4) 234)

(defn invalid-id? [id]
  (def half (/ (digits id) 2))
  (if (int? half)
    (=
      (digit-slice id 0 half)
      (digit-slice id half (digits id)))
    false))

(test (invalid-id? 10) false)
(test (invalid-id? 11) true)
(test (invalid-id? 101) false)
(test (invalid-id? 111) false)
(test (invalid-id? 696969) false)
(test (invalid-id? 1188511885) true)

(defn solve-1 [input]
  (->>
    (parse input)
    (map |(filter invalid-id? $))
    flatten
    sum))

(test (solve-1 test-input) 1227775554)

# (defn invalid-id-strict? [id]
#   (def n (length id))
#   (cond
#     (< n 2) false
#     (= ;(string/bytes id)) true
#     (< n 4) false
#     (->>
#       (range 2 (+ (/ n 2) 1))
#       (filter |(zero? (% n $)))
#       (map
#         (fn [i]
#           (=
#             (slice id 0 i)
#             ;(map |(slice id $ (+ $ i)) (range i n i)))))
#       any?)))

# (test (invalid-id-strict? 10) false)
# (test (invalid-id-strict? 11) true)
# (test (invalid-id-strict? 101) false)
# (test (invalid-id-strict? 111) true)
# (test (invalid-id-strict? 11111) true)
# (test (invalid-id-strict? 696969) true)
# (test (invalid-id-strict? 123123123) true)
# (test (invalid-id-strict? 1188511885) true)
# (test (invalid-id-strict? 2121212121) true)

# (defn solve-2 [input]
#   (->>
#     (parse input)
#     (map |(filter invalid-id-strict? $))
#     flatten
#     (map scan-number)
#     sum))

# (test (solve-2 test-input) 4174379265)

(defn main [&]
  (->>
    (file/read stdin :all)
    # (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    (|{:p1 (solve-1 $)})
    pp))
