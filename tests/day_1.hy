(import pytest)

(import [aoc.day_1 [*]])


(with-decorator (pytest.mark.parametrize
  "mass, expected"
  [[12 2]
   [14 2]
   [1969 654]
   [100756 33583]])
  (defn test-fuel-required [mass expected]
    (assert (= (fuel-required mass) expected))))

(with-decorator (pytest.mark.parametrize
  "mass, expected"
  [[1969 966]
   [100756 50346]])
  (defn test-fuel-required-recursive [mass expected]
    (assert (= (fuel-required-recursive mass) expected))))

(defn test-part-1 []
  (assert (= (part-1) 3432671)))

(defn test-part-2 []
  (assert (= (part-2) 5146132)))
