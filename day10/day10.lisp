(declaim (optimize (debug 3)))
(defpackage :day10
  (:use :cl :uiop))

(in-package :day10)

(defun load-program (file)
  (mapcar (lambda (line) (let ((parts (split-string line)))
                           (if (= 1 (length parts))
                               parts
                               (list (first parts) (parse-integer (second parts))))))
          (read-file-lines file)))


(defun calculate-results (program)
  (let ((cycle 0)
        (x 1))
    (mapcar (lambda (line) (cond
                             ((equal (first line) "noop") (list (setf cycle (+ cycle 1)) x))
                             ((equal (first line) "addx")
                              (list (setf cycle (+ cycle 2)) (setf x (+ x (second line))))))) program)))


(defun get-x-for-nth-cycle (nth seq)
  (cadar (last (remove-if (lambda (result) (>= (car result) nth)) seq))))

(apply #'+ (mapcar (lambda (n) (* n (get-x-for-nth-cycle n results))) '(20 60 100 140 180 220)))

(defun get-solution-part1 (file)
  (let* ((program (load-program file))
         (results (calculate-results program)))
    (apply #'+ (mapcar (lambda (n) (* n (get-x-for-nth-cycle n results))) '(20 60 100 140 180 220)))))

(get-solution-part1 "../resources/day10.txt")
(get-solution-part1 "../resources/puzzle10.txt")


(defun partition (input &optional (rows nil))
  (if (null input)
      rows
      (partition (subseq input 40) (append rows (list (subseq input 0 40))))))

(defun draw (pixel x) (if (<= (1- x) pixel (1+ x)) "#" "."))

(defun draw-row (row)
  (apply #'strcat (loop for pixel from 0 upto (1- (length row))
                        collect (draw pixel (nth pixel row)))))

(defun get-solution-part2 (file)
  (let* ((program (load-program file))
         (results (append '((0 1)) (calculate-results program)))
         (rows (mapcar (lambda (n) (get-x-for-nth-cycle n results))
                       (loop for n from 1 to 240 collect n))))
    (mapcar #'draw-row (partition rows))))

(get-solution-part2 "../resources/day10.txt")
(get-solution-part2 "../resources/puzzle10.txt")
