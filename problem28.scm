;;; Project Euler, problem 28 - 1001x1001 spiral
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

;; Get the north-east square
(define ne-square
  (lambda (n)
    (* n n)))

;; Get the north-west square
(define nw-square
  (lambda (n)
    (+ (- (* n n) n) 1)))

;; Get the south-east square
(define se-square
  (lambda (n)
    (+ (- (* n n) (* 3 n)) 3)))

;; Get the south-west square
(define sw-square
  (lambda (n)
    (+ (- (* n n) (* 2 n)) 2)))

(define corner-sum
  (lambda (n)
    (+ (ne-square n) (nw-square n) (se-square n) (sw-square n))))

;; Iterate through the values of an n x n spiral.
(define spiral
  (lambda (n)
    (let loop ((i 1)
               (sum 0))
      (if (> i n)
          sum
          (if (= i 1) (loop (+ i 2) 1)
              (loop (+ i 2) (+ sum (corner-sum i))))))))
              
;; Solve for a 1001x1001 spiral
(define problem-28
  (lambda ()
    (spiral 1001)))