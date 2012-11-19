;;; Project Euler, problem 5 - smallest number evenly divisible by all numbers
;;; from 1 to 20.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Create a list of numbers from 1 to n.
(define num-list
  (lambda (n)
    (if (<= n 0)
      '()
      (cons n (num-list (- n 1))))))

(define divides-all?
  (lambda (n lst)
    (if (equal? lst '())
      #t
      (if (not (zero? (modulo n (car lst))))
          #f
          (divides-all? n (cdr lst))))))

;; Create a list with all numbers in the range of 1..n.  Find the smallest
;; number that can divide each number in the list with no remainder.
(define problem-5
  (lambda (n)
    (let ((lst (num-list n)))
      (let loop ((i 1))
        (if (divides-all? i lst)
            i
            (loop (+ i 1)))))))
