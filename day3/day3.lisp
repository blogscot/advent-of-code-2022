(defpackage :day3
  (:use :cl :uiop))

(in-package :day3)

(defun read-rucksacks (file)
  (read-file-lines file))

(defun find-common-item (rucksack)
  (let* ((everything (coerce rucksack 'list))
         (compartment-length (floor (length everything) 2))
         (compartment1 (subseq everything 0 compartment-length))
         (compartment2 (subseq everything compartment-length)))
    (first (remove-duplicates (intersection compartment1 compartment2)))))

;; Lowercase item types a through z have priorities 1 through 26.
;; Uppercase item types A through Z have priorities 27 through 52.
(defun convert-to-priority (ch)
  (if (lower-case-p ch)
      (- (char-code ch) 96)
      (- (char-code ch) 38)))

(defun get-solution-part1 (file)
 (loop for rucksack in (read-rucksacks file)
       sum (convert-to-priority (find-common-item rucksack))))

;; (get-solution-part1 "data.txt")
;; (get-solution-part1 "puzzle.txt")

(defun find-badge (group)
  (first (remove-duplicates (reduce (lambda (acc rucksack) (intersection acc rucksack))
                              (mapcar (lambda (rucksack) (coerce rucksack 'list)) group)))))

(defun get-solution-part2 (file)
  (let* ((rucksacks (read-rucksacks file))
         (groups (loop for (a b c) on rucksacks by #'cdddr
       collect (list a b c))))
   (apply #'+ (mapcar #'convert-to-priority (mapcar #'find-badge groups)))))


;; (get-solution-part2 "data.txt")
;; (get-solution-part2 "puzzle.txt")
