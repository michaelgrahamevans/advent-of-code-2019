#!/usr/bin/env hy

(import [copy [deepcopy]])


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

;; Test cases
(assert (= (intcode [1 0 0 0 99]) [2 0 0 0 99]))
(assert (= (intcode [2 3 0 3 99]) [2 3 0 6 99]))
(assert (= (intcode [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
(assert (= (intcode [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99]))

(defmain []
  (with [f (open "data.txt")]
    (setv mem (list (map (fn [x] (int (x.strip))) (.split (.read f) ","))))

    (print "Part 1:" (run (deepcopy mem) 12 2))

    (for [noun (range 100)]
      (for [verb (range 100)]
        (setv part-2 (run (deepcopy mem) noun verb))
        ;(print part-2)
        (if (= part-2 19690720)
          (print "Part 2:" (+ (* 100 noun) verb)))))))
