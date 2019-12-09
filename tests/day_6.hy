(import pytest)

(import [aoc.day_6 [*]])


#@((pytest.mark.parametrize
  "orbit, expected"
  [(, "COM)A\nA)B" 3)
   (, "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L" 42)])
  (defn test-total-orbits-sum [orbit expected]
    (assert (= (total-orbits-sum (parse-map orbit))) expected)))

(defn test-part-1 []
  (assert (= (part-1) 119831)))

(defn test-part-2 []
  (assert (= (part-2) 322)))
