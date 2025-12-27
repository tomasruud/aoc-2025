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

(defn count-paths [map &opt paths n]
  (default paths @[:you])
  (default n 0)

  (var next-n n)
  (def next-paths @[])

  (each path paths
    (each branch (get map path)
      (if (= :out branch)
        (++ next-n)
        (array/push next-paths branch))))

  (if (empty? next-paths)
    next-n
    (count-paths map next-paths next-n)))

(defn solve-1 [input]
  (count-paths (parse input)))

(test (solve-1 test-input) 5)

(def test-input-2 ```
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
```)

(defn count-svr-paths [map &opt paths n]
  (default paths @[:svr])
  (default n 0)

  (var next-n n)
  (def next-paths @[])

  (each path paths
    (each branch (get map path)
      (if (= :out branch)
        (++ next-n)
        (array/push next-paths branch))))

  (if (empty? next-paths)
    next-n
    (count-svr-paths map next-paths next-n)))

(defn solve-2 [input]
  (count-svr-paths (parse input)))

(test (solve-2 test-input-2) 2)

(defn main [&]
  (->>
    (file/read stdin :all)
    (|{:p1 (solve-1 $) :p2 (solve-2 $)})
    (pp)))
