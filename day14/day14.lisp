(ql:quickload '(str))
(declaim (optimize (debug 3)))
(defpackage :day14
  (:use :cl :uiop))

(in-package :day14)

(defun 2d-array-to-list (array)
  (loop for r below (array-dimension array 0)
        collect (loop for c below (array-dimension array 1)
                      collect (aref array r c))))

(defun dump-array-to-file (file data)
  (str:to-file file (format nil "~{~{~a~}~^~%~}" (2d-array-to-list data)) :if-exists :supersede)
  nil)

(defun partition (lst)
  (loop for (a b) on lst by #'cdr
        when b
          collect (list a b)))

(defun parse-line (line)
  (loop for coordinate in (str:split " -> " line)
        collect (mapcar 'parse-integer (str:split "," coordinate))))

(defun load-paths (file)
  (mapcar 'parse-line (read-file-lines file)))

(defparameter example-file "../resources/day14.txt")
(defparameter puzzle-file "../resources/puzzle14.txt")

; Puzzle data
; The x-axis runs from 472 to 542, the y-axis runs from 13 to 176.
; Which would fit in an array 180x75 array with the x coordinates translated
; by -470.

; Example data
; The x-axis runs from 494 to 503, the y-axis runs from 4 to 9. This would fit
; in an array 11x12 with the x translated by -493

; (sort (remove-duplicates (mapcar 'first (apply #'concatenate 'list paths))) '<)
; (sort (remove-duplicates (mapcar 'second (apply #'concatenate 'list paths))) '<)

(defun draw (start end x-offset)
  (declare (special cave))
  (destructuring-bind (x1 y1) start
    (destructuring-bind (x2 y2) end
      (cond
        ((= x1 x2) (loop for y from (min y1 y2) to (max y1 y2)
                         do (setf (aref cave y (- x1 x-offset)) #\#)))
        ((= y1 y2) (loop for x from (min x1 x2) to (max x1 x2)
                         do (setf (aref cave y1 (- x x-offset)) #\#)))))))


(defun draw-cave (rows cols x-offset paths)
  (let ((cave (make-array (list rows cols) :initial-element ".")))
    (declare (special cave))
    (loop for path in paths
                do (mapc (lambda (pair)
                           (draw (first pair) (second pair) x-offset)) (partition path)))
    cave))

(defun drop-sand (cave x-offset)
  (let ((dot ".")
        (sand "o")
        (y 0)
        (x (- 500 x-offset)))
    (loop
      do (progn
           (cond
             ((= y (1- (array-dimension cave 0))) (return 'done))
             ((string= dot (aref cave (1+ y) x)) (incf y))
             ((string= dot (aref cave (1+ y) (1- x))) (progn (decf x) (incf y)))
             ((string= dot (aref cave (1+ y) (1+ x))) (progn (incf x) (incf y)))
             (t (progn
                  (setf (aref cave y x) sand)
                  (return 0))))))))

(defun get-solution-part1 (rows cols x-offset file)
  (let* ((paths (load-paths file))
         (cave (draw-cave rows cols x-offset paths)))
    (loop for i = 0 then (1+ i)
          do (if (eql 'done (drop-sand cave x-offset))
                 (return i)))))

;; (get-solution-part1 11 12 493 example-file)
;; (get-solution-part1 180 75 470 puzzle-file)
