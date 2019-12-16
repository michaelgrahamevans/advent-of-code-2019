(import [aoc.common [*]])
(import [aoc.intcode [*]])


(defn read-input []
  (with [f (open (data-file 9))]
    (.read f)))

(defn part-1 []
  (setv interpreter (Intcode (read-input) :inputs [1]))
  (list interpreter))

(defn part-2 []
  (setv interpreter (Intcode (read-input) :inputs [2]))
  (list interpreter))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
