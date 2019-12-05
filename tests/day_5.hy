(import pytest)

(import [aoc.day_5 [*]])


(defn test-part-1 []
  (assert (= (part-1) 7259358)))

#@((pytest.mark.parametrize
    "mem, input, expected"
    [(, "3,9,8,9,10,9,4,9,99,-1,8" 8 1)
     (, "3,9,8,9,10,9,4,9,99,-1,8" 0 0)])
  (defn test-intcode-part-2 [mem input expected]
    (print (last (intcode (tokenize mem) input)))
    (assert (= (last (intcode (tokenize mem) input)) expected))))

(defn test-part-2 []
  (assert (= (part-2) 11826654)))
  
