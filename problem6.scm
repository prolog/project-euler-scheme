;;; Project Euler, problem 6 - difference between square-of-sums
;;; and sum of squares.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Create a list of numbers from 1 to n.
(define num-list
  (lambda (n)
    (if (<= n 0)
      '()
      (cons n (num-list (- n 1))))))

;; The sum of the square of each list value
(define sum-of-squares
  (lambda (lst)
    (apply + (map (lambda (x) (* x x)) lst))))

;; The square of the sum of all list values.
(define square-of-sums
  (lambda (lst)
    (let ((sum (apply + lst)))
      (* sum sum))))

;; The difference between the square of sums and sum of squares for
;; the list of numbers from 1 to n.
(define problem-6
  (lambda (n)
    (let ((lst (num-list n)))
      (- (square-of-sums lst) (sum-of-squares lst)))))
