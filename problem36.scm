;;; Project Euler, problem 36.
;;;
;;; Find the sum of all numbers less than a million that are palindromic in
;;; bases 10 and 2 (decimal and binary).
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

;; Get all numbers less than n that are palindromic in bases 2 and 10.
(define palindrome-2-10
  (lambda (n)
    (if (<= n 0)
        '()
        (let* ((n-lst (number->list n))
               (n-bin (decimal->binary n))
               (n-bin-lst (number->list n-bin)))
          (if (and
               (equal? n-lst (reverse n-lst))
               (equal? n-bin-lst (reverse n-bin-lst)))
              (cons n (palindrome-2-10 (- n 1)))
              (palindrome-2-10 (- n 1)))))))

;; Find the sum of all numbers less than n that are palindromic in bases 2
;; and 10.
(define problem-36
  (lambda (n)
    (apply + (palindrome-2-10 n))))