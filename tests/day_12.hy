(import [aoc.day_12 [*]])

(import pytest)


(defn test-total-energy []
  (assert (= (total-energy [{"x" 2 "y" 1 "z" -3 "vx" -3 "vy" -2 "vz" 1}]) 36)))

#@((pytest.mark.parametrize
    "positions, steps, expected"
    [(, "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>" 10 179)])
  (defn test-simulate [positions steps expected]
    (setv moons (parse-positions positions))
    (assert (= (total-energy (simulate moons steps)) expected))))

(defn test-part-1 []
  (assert (= (part-1) 7636)))
