(import [queue [Queue]])


(defn tokenize [input]
  (list (map (fn [x] (int (x.strip))) (.split input ","))))

(defclass Intcode []
  (defn --init-- [self prog &optional [pc 0] [inputs None]]
    (setv self.pc pc)
    (setv self.mem (lfor _ (range 10000) 0))
    (setv tokens (tokenize prog))
    (for [i (range (len tokens))]
      (assoc self.mem i (get tokens i)))
    (setv self.inputs (Queue))
    (setv self.rel_base 0)
    (for [input (or inputs [])]
      (.put self.inputs input)))

  (defn --next-- [self]
    (while (not self.halted)
      (setv res (self.step))
      (unless (is res None)
        (return res)))
    (raise StopIteration))

  (defn --iter-- [self]
    (return self))

  #@(property
    (defn instruction [self]
      (get self.mem self.pc)))

  #@(property
    (defn op [self]
      (% self.instruction 100)))

  (defn mode [self pos]
    (% (// (get self.mem self.pc) (** 10 (inc pos))) 10))

  (defn arg [self pos]
    (setv x (get self.mem (+ self.pc pos)))
    (cond [(= (self.mode pos) 0) (return (get self.mem x))]
          [(= (self.mode pos) 1) (return x)]
          [(= (self.mode pos) 2) (return (get self.mem (+ x self.rel_base)))]
          [True (print "Unknown mode" (self.mode pos))]))

  (defn res-ptr [self]
    (cond [(in (self.mode 3) [0 1])
           (return (get self.mem (+ self.pc 3)))]
          [(= (self.mode 3) 2)
           (return (+ self.rel_base (get self.mem (+ self.pc 3))))]))

  (defn advance [self pos]
    (+= self.pc pos))

  #@(property
    (defn halted [self]
      (return (= self.op 99))))

  (defn step [self]
    (when self.halted
      (print "halted")
      (return))
  
    (cond
      [(= self.op 1)
        ;; add
        ;(print "add")
        (assoc self.mem (self.res-ptr) (+ (self.arg 1) (self.arg 2)))
        (self.advance 4)]
      [(= self.op 2)
        ;; multiply
        ;(print "mul")
        (assoc self.mem (self.res-ptr) (* (self.arg 1) (self.arg 2)))
        (self.advance 4)]
      [(= self.op 3)
        ;; read input
        (setv input (.get self.inputs))
        ;(print "read" input)
        (cond
          [(in (self.mode 1) [0 1])
            (assoc self.mem (get self.mem (+ self.pc 1)) input)]
          [(= (self.mode 1) 2)
            (assoc self.mem (+ (get self.mem (+ self.pc 1)) self.rel_base) input)])
        (self.advance 2)]
      [(= self.op 4)
        ;; write output
        (print "write" (self.arg 1))
        (setv res (self.arg 1))
        (self.advance 2)
        (return res)]
      [(= self.op 5)
        ;; jump-if-true
        ;(print "jump if true")
        (if (!= (self.arg 1) 0)
          (setv self.pc (self.arg 2))
          (self.advance 3))]
      [(= self.op 6)
        ;; jump-if-false
        ;(print "jump if false")
        (if (= (self.arg 1) 0)
          (setv self.pc (self.arg 2))
          (self.advance 3))]
      [(= self.op 7)
        ;; less than
        ;(print "less than")
        (if (< (self.arg 1) (self.arg 2))
          (assoc self.mem (self.res-ptr) 1)
          (assoc self.mem (self.res-ptr) 0))
        (self.advance 4)]
      [(= self.op 8)
        ;; equals
        ;(print "equals")
        (if (= (self.arg 1) (self.arg 2))
          (assoc self.mem (self.res-ptr) 1)
          (assoc self.mem (self.res-ptr) 0))
        (self.advance 4)]
      [(= self.op 9)
        ;; change relative base
        (+= self.rel_base (self.arg 1))
        (self.advance 2)]
      [(= self.op 99)
       ;; halt
       (return)]
      [True
       (print "Invalid opcode " self.op)]))

  (defn add-input [self input]
    (.put self.inputs input)))
