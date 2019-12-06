(import heapq)

(import [aoc.common [data-file]])


;; COM)A
;; A)B
;; A)C
;;
;; COM - A - B
;;        \
;;         C

;; (= (total-orbits "COM") 0)
;; (= (total-orbits "A") (inc (total-orbits "COM"))
;; (= (total-orbits "B") (inc (total-orbits "A"))
;; (= (total-orbits "C") (inc (total-orbits "A"))


(defn total-orbits-sum [nodes]
  (sum (gfor node nodes (total-orbits nodes node))))

(defn total-orbits [orbits obj]
  (if (= obj "COM")
    (return 0)
    (->> (get orbits obj) (total-orbits orbits) (inc))))

(defn build-graph [orbits]
  (setv nodes {})
  (for [orbit orbits]
    (assoc nodes (get orbit 1) (get orbit 0)))
  (return nodes))

(defn parse-map [orbit-map-str]
  (dfor orbit (gfor x (.splitlines orbit-map-str) (.split x ")")) [(get orbit 1) (get orbit 0)]))

(defn hops [nodes src dest]
  (setv src (get nodes src))
  (setv dest (get nodes dest))
  (setv src-path {src 0} src-hops 0)
  (setv dest-path {dest 0} dest-hops 0)

  (while True
    (setv src (get nodes src))
    (setv src-hops (inc src-hops))
    (if (in src dest-path)
      (do
        (return (+ src-hops (get dest-path src))))
      (assoc src-path src src-hops))

    (setv dest (get nodes dest))
    (setv dest-hops (inc dest-hops))
    (if (in dest src-path)
      (do
        (return (+ dest-hops (get src-path dest))))
      (assoc dest-path dest dest-hops))))

(defn read-input []
  (-> (data-file 6) (open) (.read)))

(defn part-1 []
  (setv nodes (-> (read-input) (parse-map)))
  (sum (gfor node nodes (total-orbits nodes node))))

(defn part-2 []
  (setv nodes (-> (read-input) (parse-map)))
  (hops nodes "YOU" "SAN"))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:" (part-2)))
