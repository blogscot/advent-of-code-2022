(declaim (optimize (debug 3)))

(defpackage :day8
  (:use :cl :uiop))

(in-package :day8)

(defun get-tree-map (file)
  (let ((grid (mapcar (lambda (line)
                        (mapcar (lambda (ch) (- (char-code ch) 48)) (coerce line 'list)))
                      (read-file-lines file))))
    (make-array (list (length grid) (length (elt grid 0))) :initial-contents grid)))


(defun tree-can-be-seen (trees)
 (let ((tree (first trees)))
   (loop for point in (rest trees)
        always (> tree point))))

(defun see-from-eastp (tree-map row col)
  (let* ((num-cols (array-dimension tree-map 1))
         (trees (loop for c from col below num-cols
                       collect (aref tree-map row c))))
    (tree-can-be-seen trees)))

(defun see-from-westp (tree-map row col)
  (let ((trees (loop for c from col downto 0
                     collect (aref tree-map row c))))
    (tree-can-be-seen trees)))

(defun see-from-southp (tree-map row col)
  (let* ((num-rows (array-dimension tree-map 0))
         (trees (loop for r from row below num-rows
                       collect (aref tree-map r col))))
    (tree-can-be-seen trees)))

(defun see-from-northp (tree-map row col)
  (let ((trees (loop for r from row downto 0
                     collect (aref tree-map r col))))
    (tree-can-be-seen trees)))

(defun get-solution-part1 (file)
  (let ((tree-map (get-tree-map file)))
   (length (loop for row from 0 below (array-dimension tree-map 0)
                 nconc (loop for col from 0 below (array-dimension tree-map 1)
                             when (or (see-from-eastp tree-map row col)
                                      (see-from-southp tree-map row col)
                                      (see-from-westp tree-map row col)
                                      (see-from-northp tree-map row col))
                               collect (list row col))))))


;; (get-solution-part1 "../resources/day8.txt")
;; (get-solution-part1 "../resources/puzzle8.txt")


(defun scenic-from-eastp (tree-map row col)
  (let ((num-cols (array-dimension tree-map 1)))
    (loop with tree = (aref tree-map row col)
          for c from (1+ col) below num-cols
          for count = 1 then (1+ count)
          until (>= (aref tree-map row c) tree)
          finally (return (or count 0)))))

(defun scenic-from-westp (tree-map row col)
  (loop with tree = (aref tree-map row col)
        for c from (1- col) downto 0
        for count = 1 then (1+ count)
        until (>= (aref tree-map row c) tree)
        finally (return (or count 0))))

(defun scenic-from-southp (tree-map row col)
  (let ((num-rows (array-dimension tree-map 0)))
    (loop with tree = (aref tree-map row col)
          for r from (1+ row) below num-rows
          for count = 1 then (1+ count)
          until (>= (aref tree-map r col) tree)
          finally (return (or count 0)))))

(defun scenic-from-northp (tree-map row col)
  (loop with tree = (aref tree-map row col)
        for r from (1- row) downto 0
        for count = 1 then (1+ count)
        until (>= (aref tree-map r col) tree)
        finally (return (or count 0))))

(defun get-solution-part2 (file)
  (let ((tree-map (get-tree-map file)))
    (loop for row from 0 below (array-dimension tree-map 0)
          collect (loop for col from 0 below (array-dimension tree-map 1)
                        collect (* (scenic-from-eastp tree-map row col)
                                   (scenic-from-southp tree-map row col)
                                   (scenic-from-westp tree-map row col)
                                   (scenic-from-northp tree-map row col)) into values
                        finally (return (apply #'max values))) into maxes
          finally (return (apply #'max maxes)))))

;; (get-solution-part2 "../resources/day8.txt")
;; (get-solution-part2 "../resources/puzzle8.txt")
