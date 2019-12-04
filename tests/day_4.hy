(import [aoc.day_4 [*]])

(import pytest)


#@((pytest.mark.parametrize
  "password, expected"
  [(, "111111" True)
   (, "223450" False)
   (, "123789" False)
   (, "235777" True)])
  (defn test-valid-password [password expected]
    (assert (= (valid-password password) expected))))

(defn test-part-1 []
  (assert (= (part-1 1178))))

#@((pytest.mark.parametrize
  "password, expected"
  [(, "112233" True)
   (, "123444" False)
   (, "111122" True)])
  (defn test-valid-password-2 [password expected]
    (assert (= (valid-password-2 password) expected))))

(defn test-part-1 []
  (assert (= (part-1 763))))

