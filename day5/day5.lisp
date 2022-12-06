(ql:quickload "cl-ppcre")

(defpackage :day5
  (:use :cl :uiop)
  (:import-from :cl-ppcre :register-groups-bind))

(in-package :day5)

(defun load-data (file)
  (let* ((lines (read-file-lines file))
         (pos (position-if #'emptyp lines))
         (crates (subseq lines 0 pos))
         (steps (subseq lines (1+ pos))))
    (list crates steps)))

(defun get-stacks (crate-data)
  (flet ((crate-number (index) (1+ (floor index 4))))
    (loop for crate in (butlast crate-data)
          append (loop for ch across crate
                       for pos = 0 then (1+ pos)
                       when (alpha-char-p ch)
                         collect (cons (crate-number pos) ch)))))

(defun get-steps (steps-data)
  (mapcar (lambda (step) (register-groups-bind (a b c)
                             ("move (\\d+) from (\\d+) to (\\d+)" step)
                           (mapcar #'parse-integer (list a b c)))) steps-data))

(defun move (stacks times from to &optional (f #'identity))
  (let* ((old (remove from stacks :key 'car :count times))
         (new (mapcar (lambda (crate) (cons to (cdr crate)))
                      (set-difference stacks old))))
    (append (funcall f new) old)))

(defun move-stacks (stacks steps f)
  (reduce (lambda (stacks step)
            (destructuring-bind (times from to) step
              (funcall f stacks times from to)))
          steps :initial-value stacks))

(defun get-solution (file f)
  (let* ((data (load-data file))
         (stacks (get-stacks (first data)))
         (steps (get-steps (second data)))
         (moved (move-stacks stacks steps f)))
    (map 'string (lambda (n) (cdr (assoc n moved)))
         (remove-duplicates (mapcar #'car stacks)))))

(defun get-solution-part1 (file)
  (get-solution file #'move))

;; (get-solution-part1 "day5.txt")
;; (get-solution-part1 "puzzle.txt")

(defun move-with-crane (stacks times from to)
  (move stacks times from to #'reverse))

(defun get-solution-part2 (file)
  (get-solution file #'move-with-crane))

;; (get-solution-part2 "day5.txt")
;; (get-solution-part2 "puzzle.txt")
