;;; Project Euler, problem 53 - combinations over 1m.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Factorial
(define fact
  (lambda (n)
    (if (<= n 0)
        1
        (* n (fact (- n 1))))))

;; Combination function
(define C
  (lambda (n r)
    (/ (fact n)
       (* (fact r) (fact (- n r))))))

;; Get the list of all combinations from 1 <= i <= n greater than cutoff.
(define combinations
  (lambda (n cutoff)
    (begin
      (let ((result-lst '()))
        (let loop ((i 1)
                   (r 1))
          (let ((combination (C i r)))
            (if (> i n)
                result-lst
                (begin
                  (if (> combination cutoff)
                      (set! result-lst (cons (list i combination) result-lst)))
                  (if (> r i)
                      (loop (+ i 1) 1)
                      (loop i (+ r 1)))))))))))

;; Get the number of combinations from 1 <= i <= n greater than cutoff.
(define problem-53
  (lambda (n cutoff)
    (length (combinations n cutoff))))
                