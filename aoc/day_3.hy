#!/usr/bin/env hy

(import os)

(import [.common [data-file]])


(defn read-input []
  (with [f (open (data-file 3))]
    (setv one (.readline f))
    (setv two (.readline f))
    (, one two)))

(defn parse-path [path]
  (defn split-direction [direction]
    ((fn [y] (, (get y 0) (int (cut y 1)))) (.strip direction)))
  (list (map split-direction (.split path ","))))

(defn parsed-path-to-coords [path]
  (setv x 0 y 0 coords [(, 0 0)])
  (for [(, direction distance) path]
    (cond
      [(= direction "U")
       (+= y distance)]
      [(= direction "D")
       (-= y distance)]
      [(= direction "R")
       (+= x distance)]
      [(= direction "L")
       (-= x distance)])
    (.append coords (, x y)))
  (return coords))

(defn segments [coords]
  "Yields (, 'V' x [y0 y1]) or (, 'H' y [x0 x1]) for each line segment."
  (setv x (get coords 0))
  (for [y (cut coords 1)]
    (cond
      [(= (get x 0) (get y 0))
       (yield (, "V" (get x 0) (sorted [(get x 1) (get y 1)])))]
      [(= (get x 1) (get y 1))
       (yield (, "H" (get x 1) (sorted [(get x 0) (get y 0)])))])
    ;(yield (, x y))
    (setv x y)))

;(defn intersections [one two]
;  "Yields a list of intersecting points between two line segments"
;  ;; https://en.wikipedia.org/wiki/Intersection_(Euclidean_geometry)#Two_lines
;
;  (for [x one]
;    (for [y two]
;      (if (x
;      (yield 0))))

(defn man-dist [coord]
  "Returns the manahattan distance from (, 0 0) to the given coordinate"
  (+ (abs (get coord 0)) (abs (get coord 1))))
  
(defn part-1 [one two])

;(defn part-1 [one two]
;  (setv one-segments (-> one (parse-path) (parsed-path-to-coords) (segments)))
;  (setv two-segments (-> two (parse-path) (parsed-path-to-coords) (segments)))
;  (for [(, d1 r1 [s1 t1]) one-segments]
;    (for [(, d2 r2 [s2 t2]) two-segments]
;      (if (= d1 d2)
;        (print "parallel")
;        (if (and (= d1 "V") (< r1 t2) (> r1 s2))
;          (yield (
;      (print d r s t))))

(defmain []
  (setv paths (read-input))
  (setv one (get paths 0))
  (setv two (get paths 1))
  (part-1 one two))
