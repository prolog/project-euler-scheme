;;; Project Euler, problem 97.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

(define problem-97 
  (lambda ()
    (let* ((mersenne-prime (number->string (+ 1 (* 28433 (expt 2 7830457)))))
           (mersenne-len (string-length mersenne-prime)))
      (substring mersenne-prime (- mersenne-len 10) mersenne-len))))