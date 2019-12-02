(import pytest)

(import [.main [*]])

(print "hello")

(defn test-fuel-required []
  (assert (= (fuel-required 12) 2))
  (assert (= (fuel-required 14) 2))
  (assert (= (fuel-required 1969) 654))
  (assert (= (fuel-required 100756) 33583)))

(defn test-fuel-required-recursive []
  (assert (= (fuel-required-recursive 1969) 966))
  (assert (= (fuel-required-recursive 100756) 50346)))

(defn test-part-1 []
  (assert (= (part-1) 3432671)))

(defn test-part-2 []
  (assert (= (part-2) 5146132)))
