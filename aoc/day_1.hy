#!/usr/bin/env hy

(import [math [floor]])
(import os)

(import [.common [data-file]])


(defn fuel-required [mass]
  (- (floor (/ mass 3)) 2))

(defn fuel-required-recursive [mass]
  (if (< mass 9)
    (return 0)
    (do
      (setv fuel (fuel-required mass))
      (return (+ fuel (fuel-required-recursive fuel))))))

(defn read-input []
  (with [f (open (data-file 1))]
    (list (map (fn [x] (-> x (.strip) (int))) (.readlines f)))))

(defn part-1 []
  (setv masses (read-input))
  (sum (map fuel-required masses)))

(defn part-2 []
  (setv masses (read-input))
  (sum (map fuel-required-recursive masses)))

(defmain []
    (print "Part 1:" (part-1))
    (print "Part 2:" (part-2)))
