(import [queue [Queue]])

(import [aoc.intcode [Intcode]])
(import [aoc.common [data-file]])


(defn read-input []
  (with [f (open (data-file 7))]
    (.read f)))

(defn amplify [program settings]
  (setv input 0)
  (for [phase settings]
    (setv interpreter (Intcode program))
    (.write interpreter phase)
    (.write interpreter input)
    (setv input (last interpreter)))
  (return input))

(defn amplify-loop [program settings]
  (setv input 0)
  (setv inputs [])

  (setv interpreters [])
  (for [x (range 5)]
    (.append interpreters (Intcode program :inputs [(get settings x)])))

  (.write (get interpreters 0) 0)

  (while True
    (for [stage (range 5)]
      (setv interpreter (get interpreters stage))
      (try
        (setv input (next interpreter))
        (except [StopIteration]
          (return input)))
      (.write (get interpreters (% (+ stage 1) 5)) input)))
  (return input))

(defn part-1 []
  (for [permutation (permutations [0 1 2 3 4])]
    (yield (amplify (read-input) permutation))))

(defn part-2 []
  (for [permutation (permutations [5 6 7 8 9])]
    (yield (amplify-loop (read-input) permutation))))

(defmain []
  (print "Part 1:" (max (part-1)))
  (print "Part 2:" (max (part-2))))
