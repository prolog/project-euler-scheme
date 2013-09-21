;;; Project Euler, problem 35 - Circular primes
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

;; Finds all the circular primes less than a given maximum.
;; 
;; Circular primes are numbers that are primes, and have the property that
;; every "rotation" formed by the number is also prime.
;;
;; E.g.: 7 is a circular prime.
;; 197 is a circular prime, because 719 and 971 are also prime.
(define circular-primes
  (lambda (max)
    (let loop ((i 1)
               ; Ascending order, for speedier lookup.
               (prime-lst (reverse (eratosthenes max)))
               (cprime-lst '()))
      (if (> i max)
          cprime-lst
          (loop (+ i 1)
                ; Speedup:
                ;
                ; If i is prime, it is the first item in the list, since
                ; the list was sorted beforehand.
                ;
                ; Discard i; it's no longer needed.
                ;
                ; Otherwise, keep the list.
                (if (null? prime-lst)
                    prime-lst
                    (if (equal? i (car prime-lst))
                        (cdr prime-lst)
                        prime-lst))
                (if (and (not (null? prime-lst)) (equal? i (car prime-lst)) (potentially-circular-prime? i) (rotations-prime? i))
                    (cons i cprime-lst)
                    cprime-lst))))))


;; Count the number of circular primes under a given maximum (the problem
;; parameters use 1000000.
(define problem-35
  (lambda (max)
    (length (circular-primes max))))