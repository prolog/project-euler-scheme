;;; Project Euler, problem 32.
;;;
;;; Find the sum of all products whose identity (multiplicand, multiplier, and 
;;; product) is 1-9 pandigital.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

;; Helper function to convert i, j, and their product into a list, which can
;; then be examined to determine whether it is 1-9 pandigital.
(define identity
  (lambda (i j)
    (let ((product (* i j)))
      (apply append (map number->list (list i j product))))))

;; Find all pandigital values in a given range, with a given starting sum,
;; and currently-found values.
;;
;; Example usage: (loop-down 9999 9999 1000 9 9 1 0 '())
(define loop-down
  (lambda (i-up i-cur i-lower j-up j-cur j-lower sum products)
    (let ((ij (* i-cur j-cur)))
      (if (equal? i-cur i-lower)
          (cons sum products)
          (if (and 
               (> i-cur j-cur) 
               (not (member ij products))
               (pandigital-lst? (identity i-cur j-cur) (num-list 9)))
              (if (equal? j-lower j-cur)
                  (loop-down i-up (- i-cur 1) i-lower j-up j-up j-lower (+ sum ij) (cons ij products))
                  (loop-down i-up i-cur i-lower j-up (- j-cur 1) j-lower (+ sum ij) (cons ij products)))
              (if (equal? j-lower j-cur)
                  (loop-down i-up (- i-cur 1) i-lower j-up j-up j-lower sum products)
                  (loop-down i-up i-cur i-lower j-up (- j-cur 1) j-lower sum products)))))))
  

;; The problem is broken up into two cases: the four digit * one digit case,
;; and the three digit * two digit case.
;; 
;; loop-down returns a pair whose car is the sum of the products of pandigital
;; identities, and whose cdr is a list containing the products found - this is
;; to eliminate duplicates.  Without this, the sum counts duplicates, and the
;; answer is too high!
(define problem-32
  (lambda ()
    (let* ((four-digits (loop-down 9999 9999 1000 9 9 1 0 '()))
           (two-digits (loop-down 999 999 100 99 99 10 0 (cdr four-digits))))
      (+ (car four-digits) (car two-digits)))))
