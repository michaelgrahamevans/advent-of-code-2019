#!/usr/bin/env hy

(import [copy [deepcopy]])
(import os)

(import [.common [data-file]])


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

(defn read-input []
  (with [f (open (data-file 2))]
    (list (map (fn [x] (int (x.strip))) (.split (.read f) ",")))))

(defn part-1 []
  (run (read-input) 12 2))

(defn part-2 []
  (setv mem (read-input))
  (for [noun (range 100)]
    (for [verb (range 100)]
      (if (= (run (deepcopy mem) noun verb) 19690720)
        (return (+ (* 100 noun) verb))))))

(defmain []
    (setv mem (read-input))
    (print "Part 1:" (part-1))
    (print "Part 2:" (part-2)))
