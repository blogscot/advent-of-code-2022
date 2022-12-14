(ql:quickload '(str))
(declaim (optimize (debug 3)))
(defpackage :day12
  (:use :cl :uiop))

(in-package :day12)

(defparameter *start-char* #\S)
(defparameter *end-char* #\E)

(defun 2d-array-to-list (array)
  (loop for r below (array-dimension array 0)
        collect (loop for c below (array-dimension array 1)
                      collect (aref array r c))))

(defun dump-array-to-file (file data)
  (str:to-file file (format nil "~{~{~a ~}~^~%~}" (2d-array-to-list data)) :if-exists :supersede))

(defun get-heightmap (file)
  "Returns an heightmap array using data in file"
  (let* ((lines (read-file-lines file))
         (heightmap (mapcar (lambda (line) (coerce line 'list)) lines)))
    (make-array (list (length heightmap) (length (first heightmap))) :initial-contents heightmap)))

(defun find-char (heightmap ch)
  "Returns the row column position of the given char in the heightmap"
  (loop named outer
        for row from 0 below (array-dimension heightmap 0)
        do (loop for col from 0 below (array-dimension heightmap 1)
                 do (when (eql ch (aref heightmap row col))
                      (return-from outer (list row col))))))

(defun find-start-pos (heightmap) (find-char heightmap *start-char*))
(defun find-end-pos (heightmap) (find-char heightmap *end-char*))

(defun neighbourp (next current)
  "Returns T if next is a valid neighbour of ch1."
  (<= (- (char-code next) (char-code current)) 1))

(defun find-neighbours (heightmap position &key (test #'neighbourp))
  "Finds neighbours of position"
  (destructuring-bind (row col) position
    (loop with letter = (aref heightmap row col)
          for (r c) in '((0 -1) (0 1) (1 0) (-1 0))
          for next-row = (+ row r)
          for next-col = (+ col c)
          when (and (<= 0 next-row (1- (array-dimension heightmap 0)))
                    (<= 0 next-col (1- (array-dimension heightmap 1)))
                    (funcall test (aref heightmap next-row next-col) letter))
            collect (list next-row next-col))))

;; Step through the map. When done read the value at the end position
(defun get-solution-part1 (heightmap)
  (let* ((start-pos (find-start-pos heightmap))
         (end-pos (find-end-pos heightmap))
         (queue (list start-pos))
         (routemap (make-array (list (array-dimension heightmap 0)
                                     (array-dimension heightmap 1))
                               :initial-element nil)))
    (setf (aref heightmap (first start-pos) (second start-pos)) #\a
          (aref heightmap (first end-pos) (second end-pos)) #\z
          (aref routemap (first start-pos) (second start-pos)) 0)
    (loop while queue
          for s = (pop queue)
          for num-steps = (aref routemap (first s) (second s))
          for neighbours = (find-neighbours heightmap s)
          do (loop for (row col) in neighbours
                   for value = (aref routemap row col)
                   when (null value)
                     do (progn
                          (setf (aref routemap row col) (1+ num-steps))
                          (setf queue (append queue (list (list row col)))))))
    (aref routemap (first end-pos) (second end-pos))))

(defparameter example-file "../resources/day12.txt")
(defparameter puzzle-file "../resources/puzzle12.txt")

(get-solution-part1 (get-heightmap example-file))
(get-solution-part1 (get-heightmap puzzle-file))

(defun find-starting-points (heightmap)
  (let* ((start-pos (find-start-pos heightmap))
         (queue (list start-pos))
         (already-seen (list start-pos)))
    (loop while queue
          for s = (pop queue)
          for neighbours = (find-neighbours heightmap s :test (lambda (next current) (declare (ignore current)) (equal next #\a)))
          do (loop for neighbour in neighbours
                   for value = (aref heightmap (first neighbour) (second neighbour))
                   unless (member neighbour already-seen :test 'equal)
                     do (progn
                          (push neighbour already-seen)
                          (push neighbour queue))))
    already-seen))

;; Start stepping from the end then select the minimum from all the possible start points
(defun get-solution-part2 (heightmap)
  (let* ((start-pos (find-start-pos heightmap))
         (end-pos (find-end-pos heightmap))
         (queue (list end-pos))
         (routemap (make-array (list (array-dimension heightmap 0)
                                     (array-dimension heightmap 1))
                               :initial-element nil))
         (starting-points (find-starting-points heightmap)))
    (setf (aref heightmap (first start-pos) (second start-pos)) #\a
          (aref heightmap (first end-pos) (second end-pos)) #\z
          (aref routemap (first end-pos) (second end-pos)) 0)
    (loop while queue
          for s = (pop queue)
          for num-steps = (aref routemap (first s) (second s))
          for neighbours = (find-neighbours heightmap s :test (lambda (next current) (<= (- (char-code current) (char-code next)) 1)))
          do (loop for (row col) in neighbours
                   for value = (aref routemap row col)
                   when (null value)
                     do (progn
                          (setf (aref routemap row col) (1+ num-steps))
                          (setf queue (append queue (list (list row col)))))))
    (apply 'min (mapcar (lambda (point) (aref routemap (first point) (second point))) starting-points))))

(get-solution-part2 (get-heightmap example-file))
(get-solution-part2 (get-heightmap puzzle-file))
