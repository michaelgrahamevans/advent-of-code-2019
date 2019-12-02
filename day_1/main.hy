#!/usr/bin/env hy

(import [math [floor]])


(defn fuel-required [mass]
  (- (floor (/ mass 3)) 2))

(defn total-fuel-required [masses]
  (sum (map fuel-required masses)))

(defn fuel-required-recursive [mass]
  (if (< mass 9)
    (return 0)
    (do
      (setv fuel (fuel-required mass))
      ;(print (+ "fuel: " (str fuel)))
      (return (+ fuel (fuel-required-recursive fuel))))))

;; Test cases
;; Part 1
(assert (= (fuel-required 12) 2))
(assert (= (fuel-required 14) 2))
(assert (= (fuel-required 1969) 654))
(assert (= (fuel-required 100756) 33583))
;; Part 2
(assert (= (fuel-required-recursive 1969) 966))
(assert (= (fuel-required-recursive 100756) 50346))

(defmain []
  (with [f (open "data.txt")]
    (setv part-1 0 part-2 0)
    (for [line (.readlines f)]
      (setv mass (int (.strip line)))
      (+= part-1 (fuel-required mass))
      (+= part-2 (fuel-required-recursive mass)))
    (print "Part 1:" part-1)
    (print "Part 2:" part-2)))
