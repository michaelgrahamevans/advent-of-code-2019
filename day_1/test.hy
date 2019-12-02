(import pytest)

(import [.main [fuel-required fuel-required-recursive]])

(print "hello")

(defn test-part-1 []
  (assert (= (fuel-required 12) 2))
  (assert (= (fuel-required 14) 2))
  (assert (= (fuel-required 1969) 654))
  (assert (= (fuel-required 100756) 33583)))

(defn test-part-2 []
  (assert (= (fuel-required-recursive 1969) 966))
  (assert (= (fuel-required-recursive 100756) 50346)))

