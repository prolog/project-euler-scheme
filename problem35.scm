;;; Project Euler, problem 35 - Circular primes
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

;; Count the number of circular primes under a given maximum (the problem
;; parameters use 1000000.
(define problem-35
  (lambda (max)
    (length (circular-primes max))))