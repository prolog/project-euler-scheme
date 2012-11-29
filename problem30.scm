;;; Project Euler, problem 30.  Find the sum of all numbers
;;; that can be written as the fifth power of their digits.
;;;
;;; Copyright (c) 2012 <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

;; The upper bound is defined below.  The reason this is the upper bound
;; is because:
;;  - the maximum value for each digit is 9^5
;;  - 6 (digits) * 9^5 is 354294, also six digits.
;;  - 7 (digits) * 9^5 is six digits, so too much.
(define fifth-upper-bound 354295)

;; Start at 10 because 1 through 9 are not technically sums, as per the
;; problem description.
(define problem-30
  (lambda ()
    (let loop ((i 10)
               (results 0))
      (if (> i fifth-upper-bound)
          results
          (if (= i (sum-of-powers i 5))
              (loop (+ i 1) (+ results i))
              (loop (+ i 1) results))))))

