;;; Project Euler, problem 67 - maximum path sum II.
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")
(load "jcd-listlib.scm")

;; The problem says there are 100 rows in the triangle.
(define problem-67
  (lambda ()
    (find-max-path-in-triangle (list->vector (file->list "triangle.txt")) 100)))