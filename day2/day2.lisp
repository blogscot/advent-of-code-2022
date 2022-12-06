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

(defparameter opponent '((a . rock) (b . paper) (c . scissors)))
(defparameter player   '((x . rock) (y . paper) (z . scissors)))
(defparameter winner-scoring  '((player1 0) (draw 3) (player2 6)))
(defparameter object-scoring  '((rock 1) (paper 2) (scissors 3)))

(defun read-rounds (file)
  (mapcar (lambda (line) (split-string line))
          (READ-file-lines file)))

(defun convert (rounds)
  (mapcar (lambda (round) (destructuring-bind (o p) round
                            (list (convert-opponent o) (convert-player p))))
          rounds))

(defun convert-opponent (item) (cdr (assoc (intern item) opponent)))
(defun convert-player (item) (cdr (assoc (intern item) player)))

(defun winner (p1 p2)
  (cond
    ((eql p1 p2) 'draw)
    ((and (eql p1 'rock) (eql p2 'paper)) 'player2)
    ((and (eql p1 'rock) (eql p2 'scissors)) 'player1)
    ((and (eql p1 'paper) (eql p2 'rock)) 'player1)
    ((and (eql p1 'paper) (eql p2 'scissors)) 'player2)
    ((and (eql p1 'scissors) (eql p2 'rock)) 'player2)
    ((and (eql p1 'scissors) (eql p2 'paper)) 'player1)))

(defun score (rounds)
  (loop for (o p) in rounds
        for object-score = (second (assoc p object-scoring))
        for winner-score = (second (assoc (winner o p) winner-scoring))
        sum (+ object-score winner-score)))

(defun get-solution-part1 (file)
  (score (convert (read-rounds file))))

;; (get-solution-part1 #p"data.txt") => 15 (4 bits, #xF, #o17, #b1111)
;; (get-solution-part1 #p"puzzle.txt") => 9651 (14 bits, #x25B3)


;; X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.


(defparameter player* '((x . lose) (y . draw) (z . win)))
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
  (let ((rounds (mapcar (lambda (round) (destructuring-bind (item result) round
                                          (list item (pairing item result))))
                        (convert* (read-rounds file)))))
    (score rounds)))

;; (get-solution-part2 #p"data.txt") => 12 (4 bits, #xC, #o14, #b1100)
;; (get-solution-part2 #p"puzzle.txt") => 10560 (14 bits, #x2940)
