(import [.common [data-file]])


(setv width 25 height 6)

(defn read-input []
  (with [f (open (data-file 8))]
    (.read f)))

(defn part-1 []
  (with [f (open (data-file 8))]
    (setv zeros (inc (* width height)) ones-twos 0)
    (while True
      (setv layer (.read f (* width height)))
      (unless (.strip layer)
        (break))
      (setv layer-zeros (.count layer "0"))
      (when (< layer-zeros zeros)
        (setv zeros layer-zeros)
        (setv ones-twos (* (.count layer "1") (.count layer "2")))))
    (return ones-twos)))

(defn part-2 []
  (with [f (open (data-file 8))]
    (setv image (lfor _ (range (* width height)) "2"))
    (while True
      (setv layer (.read f (* width height)))
      (unless (.strip layer)
        (break))
      (for [x (range (* width height))]
        (when (= (get image x) "2")
          (assoc image x (get layer x)))))
      (return (partition image width))))

(defmain []
  (print "Part 1:" (part-1))
  (print "Part 2:")
  (for [row (part-2)]
    (print (.replace (.join "" row) "1" "\x1b[34m1\x1b[0m"))))
