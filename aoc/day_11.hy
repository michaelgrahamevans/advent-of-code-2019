(import [.common [*]])
(import [.intcode [*]])

(setv WIDTH 80)

(defn read-input []
  (with [f (open (data-file 11))]
    (.read f)))

(defn coords-to-pos [x y]
  (+ (* y WIDTH) x))

(defn pos-to-coords [pos]
  {"x" (% pos WIDTH) "y" (// pos WIDTH)})

(defn paint-hull [start]
  (setv prog (Intcode (read-input)))
  (setv visited (set))

  ;; Start robot in centre
  (setv position (coords-to-pos (// WIDTH 2) (// WIDTH 2)))
  ;; Start robot facing forward
  (setv direction 0)

  ;; Hull starts all black (except for start)
  (setv hull (lfor _ (range (* WIDTH WIDTH)) 0))
  (assoc hull position start)

  (try
    (while (not prog.halted)
      ;; Send color to prog
      (setv color (get hull position))
      (.write prog color)
      ;; Get color from prog
      (setv paint (.read prog))
      (.add visited position)
      (assoc hull position paint)
      ;; Get turn from prog
      (setv turn (.read prog))
      ;; Move robot
      (assert (in turn [0 1]))
      ;; Apply turn
      (if turn
        (+= direction 1)
        (-= direction 1))
      (%= direction 4)
      ;; Move forward one
      (cond
        [(= direction 0)  ;; forward
         (-= position WIDTH)]
        [(= direction 1)  ;; right
         (+= position 1)]
        [(= direction 2)  ;; down
         (+= position WIDTH)]
        [(= direction 3)  ;; left
         (-= position 1)]))
      (except [StopIteration]))
    (return (, (len visited) hull)))

(defn part-1 []
  (get (paint-hull 0) 0))

(defn part-2 []
  (setv hull (get (paint-hull 1) 1))
  (setv out "")
  (for [y (range WIDTH)]
    (for [x (range WIDTH)]
      (if (get hull (coords-to-pos x y))
        (+= out "#")
        (+= out " ")))
    (+= out "\n"))
  (return out))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
