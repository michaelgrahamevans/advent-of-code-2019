(import pytest)

(import [aoc.day_2 [*]])


(with-decorator (pytest.mark.parametrize
  "mem, expected"
  [[[1 0 0 0 99] [2 0 0 0 99]]
   [[2 3 0 3 99] [2 3 0 6 99]]
   [[2 4 4 5 99 0] [2 4 4 5 99 9801]]
   [[1 1 1 4 99 5 6 0 99] [30 1 1 4 2 5 6 0 99]]])
  (defn test-intcode [mem expected]
    (assert (= (intcode mem) expected))))

(defn test-part-1 []
  (assert (= (part-1) 5434663)))

(defn test-part-2 []
  (assert (= (part-2) 4559)))
