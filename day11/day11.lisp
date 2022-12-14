(ql:quickload '(:cl-ppcre :str))
(declaim (optimize (debug 3)))
(defpackage :day11
  (:local-nicknames
   (:re :cl-ppcre))
  (:use :cl :uiop))

(in-package :day11)

(defun get-monkey-data (file)
  (re:split "\\n\\n" (read-file-string file)))

 (defun parse-digits (input)
  (mapcar #'parse-integer (re:all-matches-as-strings "(\\d+)" input)))

(defun parse-items (input)
  (parse-digits input))

(defun parse-divisible-by (input)
  (first (parse-digits input)))

(defun parse-throw (input)
  (first (parse-digits input)))

(defun parse-operation (input)
  (destructuring-bind (arg1 op arg2)
      (mapcar #'read-from-string (subseq (split-string (str:trim input)) 3))
    (coerce `(lambda (old) (,op ,arg1 ,arg2)) 'function)))

(defclass monkey ()
  ((items :accessor items :initarg :items)
   (operation :reader operation :initarg :operation)
   (divisible-by :reader divisible-by :initarg :divisible-by)
   (throw-on-true :reader throw-on-true :initarg :throw-on-true)
   (throw-on-false :reader throw-on-false :initarg :throw-on-false)
   (throws :accessor throws :initform 0)))

(defmethod print-object ((obj monkey) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((items items)
                     (divisible-by divisible-by)
                     (throw-on-true throw-on-true)
                     (throw-on-false throw-on-false)
                     (throws throws)) obj
      (format stream "(~a ~a ~a ~a ~a)" items divisible-by throw-on-true throw-on-false throws))))

(defun parse-monkey (monkey)
  (let ((items (parse-items (elt monkey 1)))
        (operation (parse-operation (elt monkey 2)))
        (divisible-by (parse-divisible-by (elt monkey 3)))
        (throw-on-true (parse-throw (elt monkey 4)))
        (throw-on-false (parse-throw (elt monkey 5))))
    (make-instance 'monkey :items items
                           :operation operation
                           :divisible-by divisible-by
                           :throw-on-true throw-on-true
                           :throw-on-false throw-on-false)))

(defun throw-from-to (from to item)
  (let ((current-items (items to)))
    (setf (items to) (append current-items (list item)))
    (setf (items from) nil)
    (incf (throws from))))

(defun process (state monkey worry-fn)
  (let ((items (items monkey))
        (operation (operation monkey))
        (divisible-by (divisible-by monkey)))
    (mapcar (lambda (item) (let* ((result (funcall worry-fn (funcall operation item)))
                                  (monkey-index (if (zerop (mod result divisible-by))
                                                    (throw-on-true monkey)
                                                    (throw-on-false monkey)))
                                  (next-monkey (elt state monkey-index)))
                             (throw-from-to monkey next-monkey result))) items)))

(defun get-solution-part1 (file n)
  (let* ((data (mapcar #'str:lines (get-monkey-data file)))
         (state (mapcar #'parse-monkey data))
         (worry-fn (lambda (x) (floor x 3))))
    (dotimes (i n)
      (mapcar (lambda (monkey) (process state monkey worry-fn)) state))
    (apply #'* (subseq (sort (mapcar #'throws state) #'>) 0 2))))

;; (get-solution-part1 "../resources/day11.txt" 20)
;; (get-solution-part1 "../resources/puzzle11.txt" 20)

(defun get-solution-part2 (file n)
  (let* ((data (mapcar #'str:lines (get-monkey-data file)))
         (state (mapcar #'parse-monkey data))
         (worry-value (apply #'* (mapcar #'divisible-by state)))
         (worry-fn (lambda (x) (mod x worry-value))))
    (dotimes (i n)
      (mapcar (lambda (monkey) (process state monkey worry-fn)) state))
    (apply #'* (subseq (sort (mapcar #'throws state) #'>) 0 2))))

;; (get-solution-part2 "../resources/day11.txt" 10000)
;; (get-solution-part2 "../resources/puzzle11.txt" 10000)
