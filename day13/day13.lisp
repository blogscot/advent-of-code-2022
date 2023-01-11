(ql:quickload '(cl-ppcre str))
(declaim (optimize (debug 3)))
(defpackage :day13
  (:use :cl :str)
  (:import-from :uiop :read-file-string)
  (:local-nicknames (:re :cl-ppcre)))

(in-package :day13)

(defun parse-line (line)
  (let ((matched (re:all-matches-as-strings (re:parse-string "(\\[)|(\\])|([0-9]+)") line)))
    (read-from-string (join " " (mapcar (lambda (s) (cond
                                            ((string= s "[") "(")
                                            ((string= s "]") ")")
                                            (t s))) matched)))))

(defun get-packets (file)
  (let* ((input (read-file-string file))
        (pairs (re:split "\\n\\n" input)))
    (mapcar (lambda (pair) (mapcar 'parse-line (re:split "\\n" pair))) pairs)))

(defun compare (obj1 obj2)
  (cond
    ((and (numberp obj1) (numberp obj2))
     (cond
       ((= obj1 obj2) 0)
       ((< obj1 obj2) -1)
       (t 1)))
    ((and (listp obj1) (listp obj2))
     (cond
       ((and (null obj1) (null obj2)) 0)
       ((and (null obj1) (listp obj2)) -1)
       ((and (listp obj1) (null obj2)) 1)
       (t (let ((result (compare (car obj1) (car obj2))))
            (if (zerop result)
                (compare (cdr obj1) (cdr obj2))
                result)))))
    ((numberp obj2) (compare obj1 (list obj2)))
    ((numberp obj1) (compare (list obj1) obj2))))

(defun get-solution-part1 (file)
  (let ((packets (get-packets file)))
    (loop for i = 1 then (1+ i)
          for value in (mapcar (lambda (pair) (compare (first pair) (second pair))) packets)
          when (minusp value)
            sum i)))

(defparameter example-file "../resources/day13.txt")
(defparameter puzzle-file "../resources/puzzle13.txt")

; (get-solution-part1 example-file)
; (get-solution-part1 puzzle-file)

(defun get-solution-part2 (file)
  (let* ((two '((2)))
         (six '((6)))
         (packets (get-packets file))
         (packets (append (apply 'concatenate 'list packets) (list two six)))
         (sorted (sort (copy-seq packets) (lambda (a b) (minusp (compare a b))))))
    (* (1+ (position two sorted)) (1+ (position six sorted)))))

; (get-solution-part2 example-file)
; (get-solution-part2 puzzle-file)
