#!/usr/bin/env hy

(import re)

(import [.common [data-file]])


(defn increasing [password]
  (for [c (range (dec (len password)))]
    (if (> (int (get password c)) (int (get password (inc c))))
      (return False)))
  (return True))

(defn valid-password [password]
  ;; 🤮🤮🤮
  (setv adj-re (re.compile ".*(00)|(11)|(22)|(33)|(44)|(55)|(66)|(77)|(88)|(99).*"))
    (and
      (= (len password) 6)
      (!= None (.search adj-re password))
      (increasing password)))

(defn valid-password-2 [password]
  ;; 🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
  (setv pattern (.join "|" (gfor i (range 10) (+ "((^|[^" (str i) "])" (str i) (str i) "($|[^" (str i) "]))"))))
  (setv adj-re (re.compile pattern))
  (and
    (valid-password password)
    (!= None (.search adj-re password))))

(defn part-1 []
  (setv valid 0)
  (for [i (range 235741 706949)]
    (if (valid-password (str i))
      (do
        (+= valid 1))))
  (return valid))

(defn part-2 []
  (setv valid 0)
  (for [i (range 235741 706949)]
    (if (valid-password-2 (str i))
      (do
        (+= valid 1))))
  (return valid))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
