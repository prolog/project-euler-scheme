;;; Project Euler, problem 41 - finding the largest n-digit pandigital
;;; prime.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")
(load "jcd-listlib.scm")

(define list-prime?
  (lambda (num-lst)
    (prime? (list->number num-lst))))

(define problem-41
  (lambda ()
    (let loop ((n 9)) ;; Theoretical maximum
      (let ((pandigital-primes (filter list-prime? (permute (num-list n)))))
        (if (null? pandigital-primes)
            (loop (- n 1))
            ;; Otherwise, we've found at least one pandigital prime for
            ;; the given n - sort the list, and take the highest value.
            (car (list-sort > (map list->number pandigital-primes))))))))
