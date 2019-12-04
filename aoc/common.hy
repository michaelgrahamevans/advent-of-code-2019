(import os)


(setv HERE (os.path.dirname __file__))

(defn data-file [day]
  (os.path.join HERE ".." "data" (+ "day_" (str day) ".txt")))
