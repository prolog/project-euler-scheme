;;; Project Euler, problem 34 - numbers that are the sum of the factorial
;;; of their digits.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

;; The maximum value for each digit is 9!.
;;
;; The absolute maximum occurs when  N * N! gives N digits, and
;; (N+1)*((N+1)!) gives N digits.
;;
;; (* 7 (! 9)) produces a 7 digit number
;; (* 8 (! 9)) produces a 7 digit number
;;
;; So the upper bound must be 7 * 9!
(define upper-bound (* 7 (! 9)))

;; Convert a given number into a list, apply the factorial value for each
;; digit, and sum each item in the list.
(define fact-sum
  (lambda (n fact-vec)
    (let ((num-list (number->list n)))
      (apply + (map
                (lambda (x)
                  (vector-ref fact-vec x))
                num-list)))))

;; Pre-compute the factorial of each digit, for better overall speed.
(define make-factorial-vector
  (lambda (n)
    (let ((fact-v (make-vector n)))
      (let loop ((i 0))
        (if (>= i n)
            fact-v
            (begin
              (vector-set! fact-v i (! i))
              (loop (+ i 1))))))))

;; Find the sum of all numbers that are equal to the sum of the factorial
;; of their digits.
(define problem-34
  (lambda ()
    (let ((fact-v (make-factorial-vector 10)))
      (let loop ((i 10)
                 (sum 0))
        (if (> i upper-bound)
            sum
            (if (= i (fact-sum i fact-v))
                (loop (+ i 1) (+ sum i))
                (loop (+ i 1) sum)))))))
