;;; Project Euler, problem 20 - sum of the digits of 100!
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; string-sum sums the digits of a number-as-a-string, provided that the
;; string actually represents a number.
(define string-sum
  (lambda (str strlen)
    (if (zero? strlen)
      (string->number (substring str 0 1))
      (+ (string->number (substring str strlen (+ strlen 1))) (string-sum str (- strlen 1))))))

;; Calculate a particular factorial: n! = n * (n - 1) * ... * 1
(define fact
  (lambda (n)
    (if (<= n 1)
      1
      (* n (fact (- n 1))))))

;; Find the sum of the digits in 100!
(define problem20
  (lambda ()
    (let ((fact-str (number->string (fact 100))))
      (string-sum fact-str (- (string-length fact-str) 1)))))

