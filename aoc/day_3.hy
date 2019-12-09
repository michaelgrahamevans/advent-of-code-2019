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
  (setv x 0 y 0 coords [{"x" 0 "y" 0 "dist" 0}] path-dist 0)
  (for [(, direction distance) path]
    (+= path-dist distance)
    (cond
      [(= direction "U")
       (+= y distance)]
      [(= direction "D")
       (-= y distance)]
      [(= direction "R")
       (+= x distance)]
      [(= direction "L")
       (-= x distance)])
    (.append coords {"x" x "y" y "dist" path-dist}))
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
      [(= (get one "y") (get two "y"))
       ;; ys equal, horizontal line segment
       (.append hori {
         "y" (get one "y")
         "x0" (get one "x")
         "x1" (get two "x")
         "dist0" (get one "dist")
         "dist1" (get two "dist")})]
      [(= (get one "x") (get two "x"))
       ;; xs equal, vertical line segment
       (.append vert {
         "x" (get one "x")
         "y0" (get one "y")
         "y1" (get two "y")
         "dist0" (get one "dist")
         "dist1" (get two "dist")})]))
  (, hori vert))

(defn intersection [hori vert]
  "Returns the intersection of the given horizontal and vertical line segments or None if they do not intersect."
  (if (and
        (>= (get vert "x") (min (get hori "x0") (get hori "x1")))
        (<= (get vert "x") (max (get hori "x0") (get hori "x1")))
        (>= (get hori "y") (min (get vert "y0") (get vert "y1")))
        (<= (get hori "y") (max (get vert "y0") (get vert "y1"))))
    (return {
      "x" (get vert "x")
      "y" (get hori "y")
      "dist" (+ (get hori "dist0") (abs (- (get vert "x") (get hori "x0")))
                (get vert "dist0") (abs (- (get hori "y") (get vert "y0"))))})))
  
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
  (+ (abs (get coord "x")) (abs (get coord "y"))))

(defn min-intersection [one two]
  (->> (intersections one two) (filter None) (map (fn [x] (, (man-dist x) x))) (filter (fn [x] (get x 0))) (min)))

(defn min-intersection-path [one two]
  (setv coords (filter None (intersections one two)))
  (return (min (filter None (map (fn [x] (get x "dist")) coords)))))

(defn part-1 []
  (setv paths (read-input))
  (setv one (get paths 0))
  (setv two (get paths 1))
  (get (min-intersection one two) 0))

(defn part-2 []
  (setv paths (read-input))
  (setv one (get paths 0))
  (setv two (get paths 1))
  (min-intersection-path one two))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
