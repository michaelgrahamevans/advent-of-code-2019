(import [aoc.day_3 [*]])

(import pytest)


#@((pytest.mark.parametrize
  "one, two, expected"
  [["R8,U5,L5,D3" "U7,R6,D4,L4" 6]
   ["R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" 159]
   ["U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" 135]])
  (defn test-min-intersection [one two expected]
    (assert (= (get (min-intersection one two) 0) expected))))

#@((pytest.mark.parametrize
  "one, two, expected"
  [["R8,U5,L5,D3" "U7,R6,D4,L4" 30]
   ["R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" 610]
   ["U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" 410]])
  (defn test-min-intersection-path [one two expected]
    (print (min-intersection-path one two))
    (assert (= (min-intersection-path one two) expected))))

(defn test-part-1 []
  (assert (= (part-1) 865)))

(defn test-part-2 []
  (assert (= (part-2) 35038)))
