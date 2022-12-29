(ql:quickload '(str alexandria))
(declaim (optimize (debug 3)))
(defpackage :day7
  (:use :cl :uiop)
  (:import-from :str :starts-with-p)
  (:import-from :alexandria :hash-table-values))

(in-package :day7)

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^~%~}}"
          (loop for key being the hash-keys of object
                  using (hash-value value)
                collect (list key value))))

(defun current-dir (cwd dir)
  (cond
    ((equal dir "/") '("/"))
    ((equal dir "..") (pop cwd) cwd)
    (t (push dir cwd))))

(defun get-parents (child)
  (rest (maplist 'identity child)))

(defun get-solution (file f)
  (let ((cwd nil)
        (system (make-hash-table :test 'equal))
        (lines (read-file-lines file)))
    (dolist (line lines)
      (cond
        ((starts-with-p "$ cd" line)
         (let ((dir (str:substring 5 (length line) line)))
           (setf cwd (current-dir cwd dir))
           (let ((current (gethash cwd system 0)))
             (when (zerop current)
               (setf (gethash cwd system) current)))))
        ((starts-with-p "$ ls" line))
        ((starts-with-p "dir" line))
        (t (let ((size (parse-integer (first (split-string line))))
                 (current (gethash cwd system 0)))
             (setf (gethash cwd system) (+ current size))
             (loop for parent in (get-parents cwd)
                   do (incf (gethash parent system) size))))))
    (funcall f (hash-table-values system))))

(defparameter example-file "../resources/day7.txt")
(defparameter puzzle-file "../resources/puzzle7.txt")

(defun small-values (values)
  (loop for value in values
        when (<= value 100000)
          sum value))

;; (get-solution example-file 'small-values)
;; (get-solution puzzle-file 'small-values)

(defun remove-smallest-directory (values)
  (let* ((total-disk-space 70000000)
         (required-disk-space 30000000)
         (sorted-values (sort values '>))
         (total-size (first sorted-values))
         (unused (- total-disk-space total-size))
         (required (- required-disk-space unused)))
    (first (last (remove-if (lambda (value) (< value required)) sorted-values)))))

;; (get-solution example-file 'remove-smallest-directory)
;; (get-solution puzzle-file 'remove-smallest-directory)
