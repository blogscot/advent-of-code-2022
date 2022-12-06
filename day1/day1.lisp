(ql:quickload :cl-ppcre)

(defpackage :day1
  (:use :cl :uiop)
  (:import-from :cl-ppcre :split))

(in-package :day1)

(defun group (items &optional (group nil) (groups nil))
  (if (null items)
      (mapcar (lambda (coll) (apply #'+ coll)) (append groups (list group)))
      (let ((item (first items)))
        (if (emptyp item)
            (group (rest items) nil (append groups (list group)))
            (group (rest items) (push (parse-integer item) group) groups)))))

(defun get-solution-part1 (file)
  (let ((items (read-file-lines file)))
    (apply #'max (group items))))


;; (get-solution-part1 #p"data.txt") => 24000 (15 bits, #x5DC0)
;; (get-solution-part1 #p"puzzle.txt") => 69912 (17 bits, #x11118)


(defun get-solution-part2 (file)
  (let ((items (read-file-lines file)))
    (reduce #'+ (sort (group items) #'>) :end 3)))

;; (get-solution-part2 #p"data.txt") => 45000 (16 bits, #xAFC8)
;; (get-solution-part2 #p"puzzle.txt") => 208180 (18 bits, #x32D34)

;; Solution 2 using cl-ppcre

(defun read-calories (file)
  (let ((lines (read-file-string file)))
    (mapcar (lambda (line)
              (reduce #'+ (split "\\n" line) :key #'parse-integer))
            (split "\\n\\n" lines))))

;; (apply #'max (read-calories "puzzle.txt"))
;; (reduce #'+ (sort (read-calories "puzzle.txt") #'>) :end 3)
