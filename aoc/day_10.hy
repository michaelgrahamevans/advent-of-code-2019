(import math)

(import [.common [data-file]])


(defn read-input []
  (with [f (open (data-file 10))]
        (yield-from (.readlines f))))

(defn coords [input]
  "Yields coordinates of asteroids in the given input."
  (for [(, y line)  (enumerate input)]
    (for [(, x char) (enumerate line)]
      (when (= char "#")
        (yield (, x y))))))

(defn angle-between [one two]
  "Returns the angle of the line connecting the two given points."
  (math.atan2 (- (get two 1) (get one 1)) (- (get two 0) (get one 0))))

(defn best-location [input]
  (setv best None best-count -1)
  (setv asteroids (list (coords input)))
  (for [one asteroids]
    (setv visible (set))
    (for [two asteroids]
      ;; Don't compare asteroid to itself
      (unless (= one two) 
        ;; Add angle to set of angles of visible asteroids
        ;; Asteroids with the same angle obscure each other
        (.add visible (angle-between one two))))
    (when (> (len visible) best-count)
      (setv best one best-count (len visible))))
  (return (, best best-count)))

(defn relative-dist [one two]
  (setv x1 (get one 0) x2 (get two 0))
  (setv y1 (get one 1) y2 (get two 1))
  (+ (** (- x2 x1) 2) (** (- y2 y1) 2)))

(defn vaporize [start asteroids]
  "Yields asteroids in the order they will vaporized."
  ;; Create map of asteroid angle to list of asteroid coords
  (setv targets {})
  (for [asteroid asteroids]
    (when (= start asteroid)
      (continue))
    (setv angle (angle-between asteroid start))
    (when (>= angle (/ math.pi 2))
      (setv angle (- angle (* 2 math.pi))))
    (if (in angle targets)
      (.append (get targets angle) asteroid)
      (assoc targets angle [asteroid])))

  ;; Sort asteroids by distance
  (for [value (.values targets)]
    (.sort value
      :key (fn [x] (relative-dist start x))
      :reverse True))

  ;; Yield asteroids to be vaporized
  (setv done False)
  (while (not done)
    (setv done True)
    (for [target (sorted (.keys targets))]
      (when (len (get targets target))
        (yield (.pop (get targets target)))
        (setv done False)))))

(defn part-1 []
  (best-location (read-input)))

(defn part-2 []
  (setv res (nth (vaporize (, 26 29) (coords (read-input))) 199))
  (+ (* (get res 0) 100) (get res 1)))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
