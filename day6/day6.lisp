(defpackage :day6
  (:use :cl :uiop))

(in-package :day6)

(defun get-solution (file size)
  (let ((input (first (read-file-lines file))))
    (loop for start from 0 below (- (length input) size)
          for end = (+ start size)
          for chunk = (subseq input start end)
          when (= (length (remove-duplicates (coerce chunk 'list))) size)
            do (return end))))

(defun get-solution-part1 (file)
  (get-solution file 4))

(get-solution-part1 "day6.txt")
(get-solution-part1 "puzzle.txt")

(defun get-solution-part2 (file)
  (get-solution file 14))

(get-solution-part2 "day6.txt")
(get-solution-part2 "puzzle.txt")
