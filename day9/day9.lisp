(ql:quickload '("alexandria"))
(declaim (optimize (debug 3)))

(defpackage :day9
  (:use :cl :uiop))

(in-package :day9)

(defun get-motions (file)
  (mapcar (lambda (line) (let ((motion (split-string line)))
                           (list (first motion) (parse-integer (second motion)))))
          (read-file-lines file)))

;; After each step, you'll need to update the position of the tail if the step
;; means the head is no longer adjacent to the tail.

(defun adjacentp (head tail)
  (let* ((row-diff (abs (- (first head) (first tail))))
         (col-diff (abs (- (second head) (second tail)))))
    (not (or (> row-diff 1) (> col-diff 1)))))

(defun get-distance (p1 p2)
  (let ((row-distance (- (first p1) (first p2)))
        (col-distance (- (second p1) (second p2))))
    (sqrt (+ (* row-distance row-distance) (* col-distance col-distance)))))

(defun get-neighbours (row col)
  (loop for (r c) in '((-1 -1) (-1 0) (-1 1)
                       (0 -1) (0 1)
                       (1 -1) (1 0) (1 1))
        for new-row = (+ row r)
        for new-col = (+ col c)
          collect (list new-row new-col)))

(defun find-next (head tail)
  "Returns the next tail position such that the tail is always adjacent to the head."
  (if (adjacentp head tail)
      tail
      (let* ((neighbours (get-neighbours (first tail) (second tail)))
             (adjacents (remove-if-not (lambda (n) (adjacentp head n)) neighbours)))
        (first (sort adjacents (lambda (a b) (< (get-distance head a) (get-distance head b))))))))

(defun get-moves (motions)
  (mapcan (lambda (motion)
            (destructuring-bind (direction amount) motion
              (loop repeat amount
                    collect (alexandria:switch (direction :test #'equal)
                              ("U" '(1 0))
                              ("L" '(0 -1))
                              ("D" '(-1 0))
                              ("R" '(0 1)))))) motions))

(defun add (p1 p2)
  (list (+ (first p1) (first p2)) (+ (second p1) (second p2))))

(defun get-solution-part1 (file)
  (let* ((motions (get-motions file))
         (moves (get-moves motions))
         (path '((0 0))))
    (reduce (lambda (head-pos move)
              (let* ((new-head-pos (add head-pos move))
                     (new-tail-pos (find-next new-head-pos (first path))))
                (push new-tail-pos path)
                new-head-pos)) moves :initial-value '(0 0))
    (length (remove-duplicates (reverse path) :test #'equal))))

(get-solution-part1 "../resources/day9.txt")
(get-solution-part1 "../resources/puzzle9.txt")


; move head knot of rope
; for each remaining knot calculate and update its next position
; store the position of tail knot

(defparameter *rope-length* 10)

(defun move-head (rope move)
  (let ((head (first rope)))
    (setf (first rope) (add head move))
    (first rope)))

(defun move-body ()
  (loop for a from 0 upto (- *rope-length* 2)
        for b = (1+ a)
        for next-pos = (find-next (elt rope a) (elt rope b))
        do (setf (elt rope b) next-pos)))

(defun get-solution-part2 (file)
  (let* ((motions (get-motions file))
         (moves (get-moves motions))
         (rope (loop repeat *rope-length* collect '(0 0)))
         (path '((0 0))))
    (loop for move in moves
          do (progn
               (move-head rope move)
               (move-body)
               (push (alexandria:lastcar rope) path)))
    (length (remove-duplicates path :test #'equal))))

(get-solution-part2 "../resources/day9-larger.txt")
(get-solution-part2 "../resources/puzzle9.txt")
