(defpackage :day4
  (:use :cl :uiop))

(in-package :day4)

(defun read-section-assignments (file)
  (mapcan (lambda (sections)
            (mapcar (lambda (pair)
                      (mapcar #'parse-integer (split-string pair :separator '(#\-))))
                    (split-string sections :separator '(#\,))))
          (read-file-lines file)))

(defun fully-contained-p (pairing)
  (destructuring-bind ((a b) (x y)) pairing
    (or (and (>= a x) (<= b y))
        (and (>= x a) (<= y b)))))

(defun get-solution-part1 (file)
  (let* ((assignments (read-section-assignments file))
         (pairings (loop for (a b) on assignments by #'cddr
                                   collect (fully-contained-p (list a b)))))
    (length (remove nil pairings))))

;; (get-solution-part1 "data.txt")
;; (get-solution-part1 "puzzle.txt")


(defun overlaps-p (pairing)
  (destructuring-bind ((a b) (x y)) pairing
      (intersection (loop for c from a to b collect c)
                    (loop for z from x to y collect z))))

(defun get-solution-part2 (file)
  (let* ((assignments (read-section-assignments file))
         (pairings (loop for (a b) on assignments by #'cddr
                                   collect (overlaps-p (list a b)))))
    (length (remove nil pairings))))

;; (get-solution-part2 "data.txt")
;; (get-solution-part2 "puzzle.txt")
