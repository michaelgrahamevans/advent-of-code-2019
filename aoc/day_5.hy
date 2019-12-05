#!/usr/bin/env hy

(import [copy [deepcopy]])
(import os)

(import [.common [data-file]])


(defn intcode [mem input]
  "Interprets the provided Intcode program."
  (setv pc 0)
  (while True
    (do
      (setv instruction (decode (get mem pc)))
      (setv op (get instruction "op"))
      
      (if (= op 99)
        (break))
    
      (if (in op [1 2 4 5 6 7 8])
        (do 
          (setv arg-1 (fetch mem (get instruction "mode-1") (get mem (+ pc 1))))))
      (if (in op [1 2 5 6 7 8])
        (do
          (setv arg-2 (fetch mem (get instruction "mode-2") (get mem (+ pc 2))))))
      (if (in op [1 2 3 7 8])
        (do
          (setv res-ptr (get mem (+ pc 3)))))

      (cond
        [(= op 1)
          ;; add
          (assoc mem res-ptr (+ arg-1 arg-2))
          (+= pc 4)]
        [(= op 2)
          ;; multiply
          (assoc mem res-ptr (* arg-1 arg-2))
          (+= pc 4)]
        [(= op 3)
          ;; read input
          (assoc mem res-ptr input)
          (+= pc 2)]
        [(= op 4)
          ;; Write output
          (yield arg-1)
          (+= pc 2)]
        [(= op 5)
          ;; jump-if-true
          (if (!= arg-1 0)
            (setv pc arg-2)
            (+= pc 3))]
        [(= op 6)
          ;; jump-if-false
          (if (= arg-1 0)
            (setv pc arg-2)
            (+= pc 3))]
        [(= op 7)
          ;; less than
          (if (< arg-1 arg-2)
            (assoc mem res-ptr 1)
            (assoc mem res-ptr 0))
          (+= pc 4)]
        [(= op 8)
          ;; equals
          (if (= arg-1 arg-2)
            (assoc mem res-ptr 1)
            (assoc mem res-ptr 0))
          (+= pc 4)]
        [(True)
         (print "Invalid opcode " op)])))

  (return mem))

(defn decode [instruction]
  (setv out {})
  (assoc out "op" (% instruction 100))
  (assoc out "mode-1" (% (// instruction 100) 10))
  (assoc out "mode-2" (% (// instruction 1000) 10))
  (assoc out "mode-3" (% (// instruction 10000) 10))
  (return out))

(defn fetch [mem mode val]
  (cond
    [(= mode 0)  ; position mode
     (get mem val)]
    [(= mode 1)  ; immediate mode
     (return val)]))

(defn run [mem noun verb]
  "Runs the provided Intcode program after modifying the initial noun and verb."
  (setv (get mem 1) noun)
  (setv (get mem 2) verb)
  (get (intcode mem) 0))

(defn read-input []
  (with [f (open (data-file 5))]
    (tokenize (.read f))))

(defn tokenize [input]
  (list (map (fn [x] (int (x.strip))) (.split input ","))))

(defn part-1 []
  (last (intcode (read-input) 1)))

(defn part-2 []
  (last (intcode (read-input) 5)))

(defmain []
    (setv mem (read-input))
    ;(print "Part 1:" (part-1))
    (print "Part 2:" (part-2)))
