#!/usr/bin/env hy

(import os)

(import [aoc.common [data-file]])


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
    (yield (, x y))
    (setv x y)))

(defn split-segments [segments]
  "Splits the given line segments into two lists of horizontal and vertical lines respectively."
  (setv hori [] vert [])
  (for [seg segments]
    (setv one (get seg 0) two (get seg 1))
    (cond
      [(= (get one 1) (get two 1))
       ;; ys equal, horizontal line segment
       (setv xs (sorted [(get one 0) (get two 0)]))
       (.append hori {"y" (get one 1) "x0" (get xs 0) "x1" (get xs 1)})]
      [(= (get one 0) (get two 0))
       ;; xs equal, vertical line segment
       (setv ys (sorted [(get one 1) (get two 1)]))
       (.append vert {"x" (get one 0) "y0" (get ys 0) "y1" (get ys 1)})]))
  (, hori vert))

(defn intersection [hori vert]
  "Returns the intersection of the given horizontal and vertical line segments or None if they do not intersect."
  (if (and
        (>= (get vert "x") (get hori "x0"))
        (<= (get vert "x") (get hori "x1"))
        (>= (get hori "y") (get vert "y0"))
        (<= (get hori "y") (get vert "y1")))
    (return (, (get vert "x") (get hori "y")))))
  
(defn intersections [one two]
  (setv splits-one (-> one (parse-path) (parsed-path-to-coords) (segments) (split-segments)))
  (setv splits-two (-> two (parse-path) (parsed-path-to-coords) (segments) (split-segments)))
  (setv hori-one (get splits-one 0) vert-one (get splits-one 1))
  (setv hori-two (get splits-two 0) vert-two (get splits-two 1))
  (for [hori hori-one]
    (for [vert vert-two]
      (yield (intersection hori vert))))
  (for [hori hori-two]
    (for [vert vert-one]
      (yield (intersection hori vert)))))
    
(defn man-dist [coord]
  "Returns the manahattan distance from (, 0 0) to the given coordinate"
  (+ (abs (get coord 0)) (abs (get coord 1))))

(defn min-intersection [one two]
  (->> (intersections one two) (filter None) (map man-dist) (filter None) (min)))

(defn part-1 []
  (setv paths (read-input))
  (setv one (get paths 0))
  (setv two (get paths 1))
  (min-intersection one two))

(defmain []
  (print "Part 1:" (part-1)))
