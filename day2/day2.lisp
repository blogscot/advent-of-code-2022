(defpackage :day2
  (:use :cl :uiop))

(in-package :day2)

(declaim (optimize (debug 3)))

;; A for Rock, B for Paper, and C for Scissors.
;; X for Rock, Y for Paper, and Z for Scissors.

;; The score for a single round is the score for the shape you selected
;; (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the
;; outcome of the round (0 if you lost, 3 if the round was a draw, and 6
;; if you won).

(defparameter opponent '((A . Rock) (B . Paper) (C . Scissors)))
(defparameter player   '((X . Rock) (Y . Paper) (Z . Scissors)))
(defparameter winner-scoring  '((player1 0) (draw 3) (player2 6)))
(defparameter object-scoring  '((Rock 1) (Paper 2) (Scissors 3)))

(defun read-rounds (file)
  (mapcar (lambda (line) (split-string line :separator '(#\Space)))
          (read-file-lines file)))

(defun convert (rounds)
  (mapcar (lambda (round) (destructuring-bind (o p) round
                            (list (convert-opponent o) (convert-player p))))
          rounds))

(defun convert-opponent (item) (cdr (assoc (intern item) opponent)))
(defun convert-player (item) (cdr (assoc (intern item) player)))

(defun winner (p1 p2)
  (cond
    ((eql p1 p2) 'draw)
    ((and (eql p1 'Rock) (eql p2 'Paper)) 'player2)
    ((and (eql p1 'Rock) (eql p2 'Scissors)) 'player1)
    ((and (eql p1 'Paper) (eql p2 'Rock)) 'player1)
    ((and (eql p1 'Paper) (eql p2 'Scissors)) 'player2)
    ((and (eql p1 'Scissors) (eql p2 'Rock)) 'player2)
    ((and (eql p1 'Scissors) (eql p2 'Paper)) 'player1)))

(defun get-solution-part1 (file)
  (let ((rounds (convert (read-rounds file))))
    (loop for (o p) in rounds
          for object-score = (second (assoc p object-scoring))
          for winner-score = (second (assoc (winner o p) winner-scoring))
          sum (+ object-score winner-score))))

;; (get-solution-part1 #p"data.txt") => 15 (4 bits, #xF, #o17, #b1111)
;; (get-solution-part1 #p"puzzle.txt") => 9651 (14 bits, #x25B3)


;; X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.


(defparameter player* '((X . lose) (Y . draw) (Z . win)))
(defun convert-player* (item) (cdr (assoc (intern item) player*)))

(defun convert* (rounds)
  (mapcar (lambda (round) (destructuring-bind (o p) round
                            (list (convert-opponent o) (convert-player* p))))
          rounds))

(defun pairing (p1 result)
  (cond
    ((eql result 'draw) p1 )
    ((and (eql result 'lose) (eql p1 'rock)) 'scissors)
    ((and (eql result 'lose) (eql p1 'paper)) 'rock)
    ((and (eql result 'lose) (eql p1 'scissors)) 'paper)
    ((and (eql result 'win) (eql p1 'rock)) 'paper)
    ((and (eql result 'win) (eql p1 'paper)) 'scissors)
    ((and (eql result 'win) (eql p1 'scissors)) 'rock)))

(defun get-solution-part2 (file)
  (let* ((rounds (convert* (read-rounds file)))
         (converted-rounds (mapcar (lambda (round) (destructuring-bind (item result) round
                                                     (list item (pairing item result)))) rounds)))
    (loop for (o p) in converted-rounds
          for object-score = (second (assoc p object-scoring))
          for winner-score = (second (assoc (winner o p) winner-scoring))
          sum (+ object-score winner-score))))

;; (get-solution-part2 #p"data.txt") => 12 (4 bits, #xC, #o14, #b1100)
;; (get-solution-part2 #p"puzzle.txt") => 10560 (14 bits, #x2940)
