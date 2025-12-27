(use judge)

(def test-input ```
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
```)

(defn parse [input]
  (->>
    (peg/match
      ~{:main (some (* (group :path) (any "\n")))
        :path (* :id ": " (group (* :id (any (* " " :id)))))
        :id (cmt (<- (3 :a)) ,keyword)}
      input)
    (from-pairs)))

(defn count-paths [paths &opt starts n]
  (default starts @[:you])
  (default n 0)

  (var next-n n)
  (def next-paths @[])

  (each start starts
    (each path (get paths start)
      (if (= :out path)
        (++ next-n)
        (array/push next-paths path))))

  (if (empty? next-paths)
    next-n
    (count-paths paths next-paths next-n)))

(defn solve-1 [input]
  (->>
    (parse input)
    (count-paths)))

(test (solve-1 test-input) 5)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $)})
    (pp)))
