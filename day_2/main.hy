#!/usr/bin/env hy

(import [copy [deepcopy]])
(import os)


(defn intcode [mem]
  "Interprets the provided Intcode program."
  (setv pc 0)
  (while True (do
    (setv op (get mem pc))
    (if (= op 99)
      (break))
    (setv arg-1 (get mem (get mem (+ pc 1))))
    (setv arg-2 (get mem (get mem (+ pc 2))))
    (setv res-ptr (get mem (+ pc 3)))
    (cond
      [(= op 1)
        (setv (get mem res-ptr) (+ arg-1 arg-2))]
      [(= op 2)
        (setv (get mem res-ptr) (* arg-1 arg-2))])
    (+= pc 4)))
  (return mem))

(defn run [mem noun verb]
  "Runs the provided Intcode program after modifying the initial noun and verb."
  (setv (get mem 1) noun)
  (setv (get mem 2) verb)
  (get (intcode mem) 0))

(defmain []
  (setv filename (os.path.join (os.path.dirname __file__) "data.txt"))
  (with [f (open filename)]
    (setv mem (list (map (fn [x] (int (x.strip))) (.split (.read f) ","))))

    (print "Part 1:" (run (deepcopy mem) 12 2))

    (for [noun (range 100)]
      (for [verb (range 100)]
        (setv part-2 (run (deepcopy mem) noun verb))
        ;(print part-2)
        (if (= part-2 19690720)
          (print "Part 2:" (+ (* 100 noun) verb)))))))
