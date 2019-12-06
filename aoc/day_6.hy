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


(defclass Node []
  (defn --init-- [self name]
    (setv self.name name)))

(defclass PriorityQueue []
  "Based on requirements of https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Using_a_priority_queue"
  (defn --init-- [self]
    (setv self._q [])
    (setv self._nodes {}))

  (defn add-with-priority [self node priority]
    (assoc self._nodes node.name node)
    (heapq.heappush self._q node))

  (defn decrease-priority [self node priority])

  (defn extract-min [self]
    (setv node None)
    (while (or (not node) (.removed node))
      (setv node (heapq.heappop self._q)))
    (return node)))

(defn shortest-path [nodes src dest]
  (setv pq (PriorityQueue))
  (for [node nodes]
    (.add-with-priority (Node node) 10000000)))
    


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

(defn read-input []
  (-> (data-file 6) (open) (.read)))

(defn part-1 []
  (setv nodes (-> (read-input) (parse-map)))
  (sum (gfor node nodes (total-orbits nodes node))))

(defmain []
  (print "Part 1:" (part-1)))
