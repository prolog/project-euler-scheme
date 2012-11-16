;;; Project Euler problem 48.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; For a given n, solve 1^1 + 2^2 + ... + n^n
(define exponential-sum
  (lambda (n)
    (if (<= n 0)
      0
      (+ (expt n n) (exponential-sum (- n 1))))))

;; The actual result:
(define problem-48-soln
  (lambda (n)
    (let* ((l (exponential-sum n))
	   (result-s (number->string l))
	   (result-length (string-length result-s)))
      (substring result-s (- result-length 10) result-length))))

