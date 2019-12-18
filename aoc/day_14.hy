(import [copy [deepcopy]])
(import math)

(import [aoc.common [*]])


(defn read-input []
  (with [f (open (data-file 14))]
    (.read f)))

(defn parse-reactions [input]
  (defn split-elem [elem]
    (setv x (.split elem " "))
    (assoc x 0 (int (get x 0)))
    (return x))

  (setv elements {})

  (for [line (.splitlines input)]
    (setv (, inputs output) (.split line " => "))
    (setv inputs (list (map split-elem (.split inputs ", "))))
    (setv output (split-elem output))
    (assoc elements (get output 1)
      { "amount" (get output 0) "reactants" inputs}))
  (return elements))

(defn required-ore [reactions unused product quantity]
  ;; lookup reaction for product
  (setv reaction (get reactions product))

  ;; create list of required materials
  (setv required {})
  (for [reactant (get reaction "reactants")]
    (assoc required
           (get reactant 1)
           (* (math.ceil (/ quantity (get reaction "amount"))) (get reactant 0))))

  ;; round each reactant up to multiple of (get reaction "amount")
  ;; add difference to unused
  (for [reactant required]
    (when (= reactant "ORE")
      (continue))

    (when (in reactant unused)
      (setv use (min (get required reactant) (get unused reactant)))
      (-= (get required reactant) use)
      (-= (get unused reactant) use))

    (setv quantum (get (get reactions reactant) "amount"))
    (setv min-quantity 0)
    (setv x (math.ceil (/ (get required reactant) quantum)))
    (setv min-quantity (* x quantum))
    (setv diff (- min-quantity (get required reactant)))
    (if (in reactant unused)
      (+= (get unused reactant) diff)
      (assoc unused reactant diff))
    (setv (get required reactant) min-quantity))

  ;; recursively lookup required ore for required elements, base case is ORE
  (setv total-ore 0)
  (for [reactant required]
    (if (= reactant "ORE")
      (+= total-ore (get required reactant))
      (+= total-ore (required-ore reactions
                                  unused
                                  reactant
                                  (get required reactant)))))

  (return total-ore))

(defn fuel-from-ore [reactions ore]
  ;; bounds for binary search
  (setv upper 0 lower 0)

  ;; find suitable starting bounds
  (setv i -1)
  (while True
    (+= i 1)
    (setv fuel-estimate (** 2 i))
    (setv ore-needed (required-ore reactions {} "FUEL" fuel-estimate))
    (when (> ore-needed ore)
      (setv upper (** 2 i))
      (setv lower (** 2 (dec i)))
      (break)))

  ;; binary search
  (setv prev-estimate 0)
  (while True
    (setv fuel-estimate (+ lower (// (- upper lower) 2)))
    (when (or (= prev-estimate fuel-estimate) (>= lower upper))
      (return fuel-estimate))
    (setv prev-estimate fuel-estimate)
    (setv ore-needed (required-ore reactions {} "FUEL" fuel-estimate))
    (cond [(= ore ore-needed) (return fuel-estimate)]
          [(< ore ore-needed) (setv upper fuel-estimate)]
          [(> ore ore-needed) (setv lower fuel-estimate)])))

(defn part-1 []
  (required-ore (parse-reactions (read-input)) {} "FUEL" 1))

(defn part-2 []
  (fuel-from-ore (parse-reactions (read-input)) 1000000000000))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
