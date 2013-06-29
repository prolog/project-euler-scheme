;;; Project Euler, problem 15 - Lattice paths
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

(define problem-15
  (lambda (grid-size)
    (C (* 2 grid-size) grid-size)))