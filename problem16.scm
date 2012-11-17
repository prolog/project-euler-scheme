;;; Project Euler, problem 16 - sum of 2^1000
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; string-sum sums the digits of a number-as-a-string, provided that the
;; string actually represents a number.
(define string-sum
  (lambda (str strlen)
    (if (zero? strlen)
      (string->number (substring str 0 1))
      (+ (string->number (substring str strlen (+ strlen 1))) (string-sum str (- strlen 1))))))

(define problem-16
  (lambda ()
    (let ((val (number->string (expt 2 1000))))
      (string-sum val (- (string-length val) 1)))))
