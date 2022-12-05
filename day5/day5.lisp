(defpackage :day5
  (:use :cl :uiop)
  (:import-from :str :split)
  (:import-from :cl-ppcre :register-groups-bind))

(in-package :day5)

;; (ql:quickload :str)
;; (ql:quickload "cl-ppcre")

(defun load-data (file)
  (let* ((lines (read-file-lines file))
         (pos (position-if #'emptyp lines))
         (crates (subseq lines 0 pos))
         (steps (subseq lines (1+ pos))))
    (list crates steps)))

(defun crate-number (index) (1+ (floor index 4)))

(defun get-stacks (crate-data)
  (let ((stacks (loop for crate in (butlast crate-data)
                      append (loop for ch across crate
                                   for pos = 0 then (1+ pos)
                                   when (alpha-char-p ch)
                                     collect (cons (crate-number pos) ch)))))
    (stable-sort stacks #'< :key #'car)))

(defun get-steps (steps-data)
  (mapcar (lambda (step) (register-groups-bind (a b c)
                             ("move (\\d+) from (\\d+) to (\\d+)" step)
                           (mapcar #'parse-integer (list a b c)))) steps-data))

(defun move-crate (stacks from to)
  (let* ((old-crate (assoc from stacks))
         (new-crate (cons to (cdr old-crate))))
    (append (list new-crate) (remove old-crate stacks))))

(defun move (stacks times from to)
  (loop for n from 0 to times
        for new-stacks = stacks then (move-crate new-stacks from to)
        finally (return new-stacks)))

(defun move-stacks (stacks steps)
  (reduce (lambda (stacks step)
            (destructuring-bind (times from to) step
              (move stacks times from to)))
          steps :initial-value stacks))

(defun get-solution-part1 (file)
  (let* ((data (load-data file))
         (stacks (get-stacks (first data)))
         (steps (get-steps (second data)))
         (moved (move-stacks stacks steps)))
    (map 'string (lambda (n) (cdr (assoc n moved)))
         (remove-duplicates (mapcar #'car stacks)))))

;; (get-solution-part1 "day5.txt")
;; (get-solution-part1 "puzzle.txt")

(defun move* (stacks times from to)
  (let* ((old (remove from stacks :key 'car :count times))
         (new (mapcar (lambda (crate) (cons to (cdr crate)))
                      (set-difference stacks old))))
    (stable-sort (append (reverse new) old) #'< :key #'car)))

(defun move-stacks* (stacks steps)
  (reduce (lambda (stacks step)
            (destructuring-bind (times from to) step
              (move* stacks times from to)))
          steps :initial-value stacks))

(defun get-solution-part2 (file)
  (let* ((data (load-data file))
         (stacks (get-stacks (first data)))
         (steps (get-steps (second data)))
         (moved (move-stacks* stacks steps)))
    (map 'string (lambda (n) (cdr (assoc n moved)))
         (remove-duplicates (mapcar #'car stacks)))))

;; (get-solution-part2 "day5.txt")
;; (get-solution-part2 "puzzle.txt")
