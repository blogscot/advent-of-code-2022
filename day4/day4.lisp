(defpackage :day4
  (:use :cl)
  (:import-from :uiop :read-file-lines)
  (:import-from :str :split))

;; (ql:quickload :str)

(in-package :day4)

(defun read-section-assignments (file)
  (mapcan (lambda (sections)
            (mapcar (lambda (pair)
                      (mapcar #'parse-integer (split "-" pair)))
                    (split "," sections)))
          (read-file-lines file)))

(defun fully-contained-p (pairing)
  (destructuring-bind ((a b) (x y)) pairing
    (or (and (>= a x) (<= b y))
        (and (>= x a) (<= y b)))))

(defun get-solution (file f)
  (let* ((assignments (read-section-assignments file))
         (pairings (loop for (a b) on assignments by #'cddr
                                   collect (funcall f (list a b)))))
    (length (remove nil pairings))))

(defun get-solution-part1 (file)
  (get-solution file #'fully-contained-p))

;; (get-solution-part1 "data.txt")
;; (get-solution-part1 "puzzle.txt")


(defun overlaps-p (pairing)
  (destructuring-bind ((a b) (x y)) pairing
      (intersection (loop for c from a to b collect c)
                    (loop for z from x to y collect z))))

(defun get-solution-part2 (file)
  (get-solution file #'overlaps-p))


;; (get-solution-part2 "data.txt")
;; (get-solution-part2 "puzzle.txt")
