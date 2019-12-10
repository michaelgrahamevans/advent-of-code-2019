(import pytest)

(import [aoc.day_10 [*]])


(setv SMALL
  ["......#.#."
   "#..#.#...."
   "..#######."
   ".#.#.###.."
   ".#..#....."
   "..#....#.#"
   "#..#....#."
   ".##.#..###"
   "##...#..#."
   ".#....####"])

(setv BIG
  [".#..##.###...#######"
   "##.############..##."
   ".#.######.########.#"
   ".###.#######.####.#."
   "#####.##.#.##.###.##"
   "..#####..#.#########"
   "####################"
   "#.####....###.#.#.##"
   "##.#################"
   "#####.##.###..####.."
   "..######..##.#######"
   "####.##.####...##..#"
   ".#####..#.######.###"
   "##...#.##########..."
   "#.##########.#######"
   ".####.#.###.###.#.##"
   "....##.##.###..#####"
   ".#.#.###########.###"
   "#.#.#.#####.####.###"
   "###.##.####.##.#..##"])

#@((pytest.mark.parametrize
    "input, expected"
    [(, SMALL (, (, 5 8) 33))
     (, BIG (, (, 11 13) 210))])
  (defn test-best-location [input expected]
    (assert (= (best-location input) expected))))

(defn test-part-1 []
  (print (part-1))
  (assert (= (part-1) (, (, 26 29) 303))))

(defn test-vaporize []
  (setv targets (list (vaporize (, 11 13) (coords BIG))))
  (for [x targets]
    (print x))
  (assert (= (nth targets 0) (, 11 12)))
  (assert (= (nth targets 199) (, 8 2))))

(defn test-part-2 []
  (assert (= (part-2) 408)))
