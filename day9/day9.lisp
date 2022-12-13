(ql:quickload '("fiveam" "alexandria"))
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
                              ("R" '(0 1))))
              )) motions))

(defun get-solution-part1 (file)
  (let* ((motions (get-motions file))
         (moves (get-moves motions))
         (path '((0 0))))
    (reduce (lambda (head-pos move)
              (let* ((new-head-pos (list (+ (first head-pos) (first move))
                                         (+ (second head-pos) (second move))))
                     (new-tail-pos (find-next new-head-pos (first path))))
                (push new-tail-pos path)
                new-head-pos)) moves :initial-value '(0 0))
    (length (remove-duplicates (reverse path) :test #'equal))))

(get-solution-part1 "../resources/day9.txt")
(get-solution-part1 "../resources/puzzle9.txt")
