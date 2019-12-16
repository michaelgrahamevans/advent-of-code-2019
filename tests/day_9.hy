(import pytest)

(import [aoc.intcode [*]])
(import [aoc.day_9 [*]])


#@((pytest.mark.parametrize
    "prog, expected"
    [(, "104,1125899906842624,99" [1125899906842624])
     (, "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
         [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99])])
  (defn test-intocode [prog expected]
    (setv interpreter (Intcode prog))
    (assert (= (list interpreter) expected))))

(defn test-part-1 []
  (assert (= (part-1) [4261108180])))

(defn test-part-2 []
  (assert (= (part-2) [77944])))
