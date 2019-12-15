(import [copy [deepcopy]])
(import math)
(import re)

(import [.common [*]])


(defn read-input []
  (with [f (open (data-file 12))]
    (parse-positions (.read f))))

(defn parse-positions [positions]
    (for [(, i line) (enumerate (.splitlines positions))]
      (setv (, x y z) (->> line (re.search r"x=(-?\d+).*y=(-?\d+).*z=(-?\d+)") (.groups)))
      (yield {"id" i
              "x" (int x)
              "y" (int y)
              "z" (int z)
              "vx" 0
              "vy" 0
              "vz" 0})))

(defn gravity [moons]
  (for [one moons]
    (setv new (deepcopy one))
    (for [two moons]
      ;; Don't compare moon to itself
      (when (= (get one "id") (get two "id"))
        (continue))
      (for [axis ["x" "y" "z"]]
        ;(assoc new axis (get one axis))
        (setv v-axis (+ "v" axis))
        (cond
          [(< (get one axis) (get two axis))
           (assoc new v-axis (inc (get new v-axis)))]
          [(> (get one axis) (get two axis))
           (assoc new v-axis (dec (get new v-axis)))]
          [True
           (assoc new v-axis (get new v-axis))])))
    (yield new)))
      
(defn update-velocity [moons]
  (for [moon moons]
    (for [axis ["x" "y" "z"]]
      (+= (get moon axis) (get moon (+ "v" axis))))))

(defn total-energy [moons]
  (setv total 0)
  (for [moon moons]
    (setv potential 0 kinetic 0)
    (for [axis ["x" "y" "z"]]
      (+= potential (abs (get moon axis)))
      (+= kinetic (abs (get moon (+ "v" axis)))))
    (+= total (* potential kinetic)))
  total)

(defn format-moon [moon]
  (return (.format "pos=<x={x:3}, y={y:3}, z={z:3}>, vel=<x={vx:3}, y={vy:3}, z={vz:2}>" #**moon)))

(defn print-moons [moons]
  (for [moon moons]
    (print (format-moon moon)))
  (print ""))

(defn simulate [moons steps]
  (setv moons (tuple moons))
  (for [step (range steps)]
    (setv moons (tuple (gravity moons)))
    (update-velocity moons))
  moons)

(defn snapshot-axis [moons axis]
  (tuple (gfor moon moons (, (get moon axis) (get moon (+ "v" axis))))))

(defn part-1 []
  (-> (read-input) (simulate 1000) (total-energy)))

(defn part-2 []
  (setv res {"x" 0 "y" 0 "z" 0})
  (setv seen {"x" {} "y" {} "z" {}})

  (setv initial (tuple (read-input)))
  (setv new (deepcopy initial))

  (setv steps 0)

  (for [axis ["x" "y" "z"]]
    (setv snapshot (snapshot-axis new axis))
    (assoc (get seen axis) snapshot steps))

  (while True
    (when (and (get res "x") (get res "y") (get res "z"))
      (break))
    (+= steps 1)
    (setv new (simulate new 1))
    (for [axis ["x" "y" "z"]]
      (setv snapshot (snapshot-axis new axis))
      (when (in snapshot (get seen axis))
        (when (= (get res axis) 0)
          (assoc res axis steps)))
      (assoc (get seen axis) snapshot steps)))

  (lcm (get res "x") (get res "y") (get res "z")))

(defn lcm [&rest args]
  (if (= (len args) 2)
    (return (int (/ (* (get args 0) (get args 1))
                 (math.gcd (get args 0) (get args 1)))))
    (return (lcm (get args 0) (lcm #*(cut args 1))))))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
