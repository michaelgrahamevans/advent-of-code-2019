(import [aoc.common [*]])
(import [aoc.intcode [*]])


(setv WIDTH 42)
(setv HEIGHT 24)

(defn read-input []
  (with [f (open (data-file 13))]
    (.read f)))

(defn coords-to-pos [x y]
  (+ (* y WIDTH) x))

(defn print-buffer [buffer]
  (for [y (range HEIGHT)]
    (for [x (range WIDTH)]
      (setv tile-id (get buffer (coords-to-pos x y)))
      (setv tile (cond
                   [(= tile-id 0) " "]
                   [(= tile_id 1) "$"]
                   [(= tile-id 2) "#"]
                   [(= tile-id 3) "_"]
                   [(= tile-id 4) "*"]))
      (print tile :end ""))
    (print ""))
  (print "")
  (for [x (range WIDTH)]
    (print (// x 10) :end ""))
  (print "")
  (for [x (range WIDTH)]
    (print (% x 10) :end ""))
  (print ""))

(defn part-1 []
  (setv prog (Intcode (read-input)))
  (setv buffer (lfor _ (range (* WIDTH HEIGHT)) 0))
  (try
    (while (not prog.halted)
      (setv x (.read prog))
      (setv y (.read prog))
      (setv tile-id (.read prog))
      (assoc buffer (+ (* y WIDTH) x) tile-id))
    (except [StopIteration]))
  ;(print-buffer buffer)
  (return (.count buffer 2)))

(defn part-2 []
  (setv prog (Intcode (read-input)))
  (assoc prog.mem 0 2)  ;; Play for free
  (setv buffer (lfor _ (range (* WIDTH HEIGHT)) 0))
  (setv prev-ball-pos 0 ball-pos 0 joy-pos 0 score 0)

  (try
    (while (not prog.halted)
      (setv x (.read prog))
      (setv y (.read prog))
      (setv tile-id (.read prog))
      (if (and (= x -1) (= y 0))
        (do
          (setv score tile-id))
        (do
          (assoc buffer (coords-to-pos x y) tile-id)
          ;; Update state
          (when (= tile-id 3)
            (setv joy-pos x))
          (when (= tile-id 4)
            (setv joy-dir 0)
            (setv prev-ball-pos ball-pos)
            (setv ball-pos x)
            ;(print-buffer buffer)

            ;; Decide new joystick position
            (setv target ball-pos)
            (setv joy-dir
              (cond
                [(< joy-pos target) 1]
                [(> joy-pos target) -1]
                [(= joy-pos target) 0]))
            (.write prog joy-dir)))))
    (except [StopIteration]))
  (return score))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
