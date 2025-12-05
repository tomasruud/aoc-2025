(use judge)

(def test-input ```
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
```)

(defn id-range
  "Generates a full range of string IDs, with hi inclusive"
  [lo hi]
  (map string (range lo (+ 1 hi))))

(test (id-range 1 5) @["1" "2" "3" "4" "5"])

(defn parse [input]
  (peg/match
    ~{:main (some (* (cmt :range ,id-range) (any ",") (any "\n")))
      :range (* (number :d+) "-" (number :d+))}
    input))

(test (get (parse test-input) 10)
      @["2121212118"
        "2121212119"
        "2121212120"
        "2121212121"
        "2121212122"
        "2121212123"
        "2121212124"])

(defn invalid-id? [id]
  (and
    (>= (length id) 2)
    (= ;(partition (math/ceil (/ (length id) 2)) id))))

(test (invalid-id? "10") false)
(test (invalid-id? "11") true)
(test (invalid-id? "101") false)
(test (invalid-id? "111") false)
(test (invalid-id? "696969") false)
(test (invalid-id? "1188511885") true)

(defn solve-1 [input]
  (->>
    (parse input)
    (map |(filter invalid-id? $))
    flatten
    (map scan-number)
    sum))

(test (solve-1 test-input) 1227775554)

(defn invalid-id-strict? [id]
  (and
    (>= (length id) 2)
    (->>
      (range 0 (math/ceil (/ (length id) 2)))
      (map |(= ;(partition (+ $ 1) id)))
      any?)))

(test (invalid-id-strict? "10") false)
(test (invalid-id-strict? "11") true)
(test (invalid-id-strict? "101") false)
(test (invalid-id-strict? "111") true)
(test (invalid-id-strict? "11111") true)
(test (invalid-id-strict? "696969") true)
(test (invalid-id-strict? "123123123") true)
(test (invalid-id-strict? "1188511885") true)
(test (invalid-id-strict? "2121212121") true)

(defn solve-2 [input]
  (->>
    (parse input)
    (map |(filter invalid-id-strict? $))
    flatten
    (map scan-number)
    sum))

(test (solve-2 test-input) 4174379265)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    pp))
