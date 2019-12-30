(import [aoc.common [*]])


(defn read-input []
  (with [f (open (data-file 24))]
    (return (.read f))))

(defn parse-input [input]
  (setv buffer (lfor _ (range 25) 0))
  (setv i 0)
  (for [line (.splitlines input)]
    (for [char line]
      (when (= char "#")
        (assoc buffer i 1))
      (+= i 1)))
  (return buffer))

(defn step [initial-state]
  (setv next-state (lfor _ (range 25) 0))
  (for [x (range 25)]
    (setv adjacent 0)
    (unless (= (% x 5) 0)  ;; Left edge
      (+= adjacent (get initial-state (- x 1))))
    (unless (= (% x 5) 4)  ;; Right edge
      (+= adjacent (get initial-state (+ x 1))))
    (unless (< x 5)  ;; Top edge
      (+= adjacent (get initial-state (- x 5))))
    (unless (>= x 20)  ;; Bottom edge
      (+= adjacent (get initial-state (+ x 5))))

    (setv current (get initial-state x))
    (assoc next-state x current)
    (when (and (= current 1)  ;; Alive
               (!= adjacent 1))
          (assoc next-state x 0))  ;; Dies
    (when (and (= current 0)  ;; Empty
               (in adjacent [1 2]))
          (assoc next-state x 1)))
  (return next-state))

(defn bio-rating [buffer]
  (setv rating 0)
  (for [(, i j) (enumerate buffer)]
    (when (= j 1)
      (+= rating (** 2 i))))
  (return rating))


(defclass Node []
  (defn --init-- [self &optional [buffer None] [inner None] [outer None]]
    (setv self.inner inner)
    (setv self.outer outer)
    (setv self.buffer (or buffer (lfor _ (range 25) 0)))))

;; 0  1  2  3  4
;; 5  6  7  8  9
;; 10 11 12 13 14
;; 15 16 17 18 19
;; 20 21 22 23 24

(defn iter-edges [buffer]
  (for [x (range 25)]
    (when (or (= (% x 5) 0) (= (% x 5) 4) (< x 5) (>= x 20))
      (yield (get buffer x)))))

(defn iter-top-edges [buffer]
  (for [x (range 25)]
    (when (< x 5)
      (yield (get buffer x)))))

(defn iter-bottom-edges [buffer]
  (for [x (range 25)]
    (when (>= x 20)
      (yield (get buffer x)))))

(defn iter-left-edges [buffer]
  (for [x (range 25)]
    (when (= (% x 5) 0)
      (yield (get buffer x)))))

(defn iter-right-edges [buffer]
  (for [x (range 25)]
    (when (= (% x 5) 4)
      (yield (get buffer x)))))

(defn step-recur [state &optional [top 0] [bottom 0] [left 0] [right 0]]
  (setv next-state (Node))
  (assoc state.buffer 12 0)
  (for [x (range 25)]
    (when (= x 12)
      (continue))

    (setv adjacent 0)

    (if (= (% x 5) 0)  ;; Left edge
      (+= adjacent left)
      (+= adjacent (get state.buffer (- x 1))))
    (if (= (% x 5) 4)  ;; Right edge
      (+= adjacent right)
      (+= adjacent (get state.buffer (+ x 1))))
    (if (< x 5)  ;; Top edge
      (+= adjacent top)
      (+= adjacent (get state.buffer (- x 5))))
    (if (>= x 20)  ;; Bottom edge
      (+= adjacent bottom)
      (+= adjacent (get state.buffer (+ x 5))))

    ;; Inner edges
    (when state.inner
      (cond [(= x 11) ;; Left inner
             (+= adjacent (sum (iter-left-edges state.inner.buffer)))]
            [(= x 13) ;; Right inner
             (+= adjacent (sum (iter-right-edges state.inner.buffer)))]
            [(= x 7) ;; Top inner
             (+= adjacent (sum (iter-top-edges state.inner.buffer)))]
            [(= x 17) ;; Bottom inner
             (+= adjacent (sum (iter-bottom-edges state.inner.buffer)))]))

    (setv current (get state.buffer x))
    (assoc next-state.buffer x current)
    (when (and (= current 1)  ;; Alive
               (!= adjacent 1))
          (assoc next-state.buffer x 0))  ;; Dies
    (when (and (= current 0)  ;; Empty
               (in adjacent [1 2]))
          (assoc next-state.buffer x 1)))

  (if state.inner 
    (setv next-state.inner (step-recur state.inner
                                       :top (get state.buffer 7)
                                       :bottom (get state.buffer 17)
                                       :left (get state.buffer 11)
                                       :right (get state.buffer 13)))
    (when (any (gfor x [7 17 11 13] (get state.buffer x)))
      (setv next-state.inner (step-recur (Node)
                                         :top (get state.buffer 7)
                                         :bottom (get state.buffer 17)
                                         :left (get state.buffer 11)
                                         :right (get state.buffer 13)))))
  (return next-state))

(defn count-recur [state]
  (setv total 0)
  (while state
    (+= total (sum state.buffer))
    (setv state state.inner))
  (return total))

(defn print-buffer [buffer]
  (for [(, i j) (enumerate buffer)]
    (if (= j 1)
      (print "#" :end "")
      (print "." :end ""))
    (when (= (% i 5) 4)
      (print ""))))

(defn print-recur [node]
  (while node
    (print-buffer node.buffer)
    (print "")
    (setv node node.inner))
  (print ""))
  
(defn part-1 []
  ;(setv buffer (parse-input "....#\n#..#.\n#..##\n..#..\n#...."))
  (setv buffer (parse-input (read-input)))
  (setv seen (set))
  (while True
    (setv rating (bio-rating buffer))
    (when (in rating seen)
      (return rating))
    (.add seen rating)
    (setv buffer (step buffer))))

(defn part-2 []
  ;(setv buffer (parse-input "....#\n#..#.\n#..##\n..#..\n#...."))
  (setv buffer (parse-input (read-input)))
  (setv state (Node :buffer buffer))
  (for [_ (range 200)]
    (setv state (Node :inner state))
    (setv state (step-recur state)))
  (return (count-recur state)))
  

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
