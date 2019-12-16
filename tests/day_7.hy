(import [queue [Queue]])

(import pytest)

(import [aoc.day_7 [*]])
(import [aoc.intcode [Intcode]])


#@((pytest.mark.parametrize
    "program, settings, expected"
    [(, "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" [4 3 2 1 0] 43210)
     (, "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" [0 1 2 3 4] 54321)
     (, "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" [1 0 4 3 2] 65210)])
  (defn test-amplify [program settings expected]
    (print (amplify program settings))
    (assert (= (amplify program settings) expected))))

(defn test-part-1 []
  (assert (= (max (part-1)) 22012)))

#@((pytest.mark.parametrize
    "in_mem, out_mem, expected"
    [(, [3 5 4 0 99 0] [3 5 4 0 99 10] 3)])
  (defn test-intcode [in-mem out-mem expected]
    (setv prog (.join "," (map str in-mem)))
    (setv interpreter (Intcode prog :inputs [10]))
    (setv result (last interpreter))
    (assert (= result expected))
    (assert (= (cut interpreter.mem 0 6) out-mem))))

#@((pytest.mark.parametrize
    "program, settings, expected"
    [(, "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" [9 8 7 6 5] 139629729)
     (, "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" [9 7 8 5 6] 18216)])
  (defn test-amplify-loop [program settings expected]
    (print (amplify-loop program settings))
    (assert (= (amplify-loop program settings) expected))))

(defn test-part-2 []
  (assert (= (max (part-2)) 4039164)))
