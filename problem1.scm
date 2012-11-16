;;; Project Euler problem 1 - find the sum of all the multiples of 3 or 5
;;; below 1000.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Check to see if a given number is either a multiple of 3 or 5.
(define mult-3-or-5?
  (lambda (n)
    (if (or
	  (= 0 (modulo n 3))
	  (= 0 (modulo n 5))
	)
    #t
    #f)))

;; Create a list of numbers from 1 to n.
(define num-list
  (lambda (n)
    (if (<= n 0)
      '()
      (cons n (num-list (- n 1))))))


;; Find the sum of all natural numbers < 1000 that are multiples of 3 or 5.
(apply + (filter mult-3-or-5? (num-list 999)))
