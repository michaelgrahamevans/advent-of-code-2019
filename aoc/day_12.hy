(import [copy [deepcopy]])
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

(defn part-1 []
  (-> (read-input) (simulate 1000) (total-energy)))

(defn part-2 []
  (setv initial (tuple (read-input)))
  ;(setv initial (tuple (parse-positions "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")))
  (setv seen-states (set [(tuple (gfor x initial (tuple (.values x))))]))
  ;(print-moons initial)
  (setv new (deepcopy initial))
  (setv steps 0)
  (setv max-x 0)
  (while True
    (+= steps 1)
    (setv new (simulate new 1))
    (print-moons new)
    (setv max-x (max max-x (get "x" (get new 0))))
    (print max-x)
    (setv new-values (tuple (gfor x new (tuple (.values x)))))
    (when (in new-values seen-states)
      (return steps))
    (.add seen-states new-values)))

(defmain []
  (print "Part 1:" (part-1)))
  ;(print "Part 2:" (part-2)))
