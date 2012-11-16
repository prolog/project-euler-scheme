;;; Project Euler, problem 2.  Find the sum of all even-valued terms of
;;; the Fibonacci sequence whose values do not exceed 4000000.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Calculate a particular Fibonacci number.
(define fibonacci
  (lambda (n)
    (case n
      ((0 1) 1)
      (else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))

;; Create a list of Fibonacci numbers
(define fibonacci-list
  (lambda (n)
   (do ((vec (make-vector n))
     (i 1 (+ i 1)))
     ((= i n) vec)
     (vector-set! vec i (fibonacci i)))))

;; fib(34) is 3524578...
(apply + (filter even? (vector->list (fibonacci-list 34))))
