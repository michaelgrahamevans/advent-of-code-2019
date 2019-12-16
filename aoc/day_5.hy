#!/usr/bin/env hy

(import [copy [deepcopy]])
(import os)

(import [.intcode [Intcode]])
(import [.common [data-file]])


(defn read-input []
  (with [f (open (data-file 5))]
    (.read f)))

(defn tokenize [input]
  (list (map (fn [x] (int (x.strip))) (.split input ","))))

(defn part-1 []
  (setv interpreter (Intcode (read-input) :inputs [1]))
  (last interpreter))

(defn part-2 []
  (setv interpreter (Intcode (read-input) :inputs [5]))
  (last interpreter))

(defmain []
    (print "Part 1:" (part-1))
    (print "Part 2:" (part-2)))
