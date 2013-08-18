;;; Project Euler problem 27 - quadratic primes.
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

;; Compute a quadratic of the form n^2 + an + b
(define quadr-euler
  (lambda (n a b)
    (+ (* n n) (* a n) b)))

(define incr-a
  (lambda (a b b-max)
    (if (= b b-max)
        (+ a 1)
        a)))

(define incr-b
  (lambda (b b-min b-max)
    (if (= b b-max)
        b-min
        (+ b 1))))

;; Generate a list of primes from n=0, ... for the given coefficients a and
;; b, stopping when we get a number that is not a prime.
(define quadr-prime-list
  (lambda (a b)
    (let loop ((n 0)
               (prime-lst '()))
      (let ((cur-val (quadr-euler n a b)))
        (if (and (> cur-val 1) (prime? cur-val))
            (loop (+ n 1) (cons cur-val prime-lst))
            prime-lst)))))

;; Returns a list containing the first co-efficient, the second co-efficient,
;; and the list of consecutive primes starting at n=0.  This list represents
;; the co-efficients with the largest number of generated primes within the
;; given problem parameters.  
(define coefficients-and-max-consec-primes
  (lambda (a-orig a-max b-orig b-max)
    (let loop ((a a-orig)
               (b b-orig)
               (lst '('none 'none '())))
      (if (> a a-max)
          lst
          (let ((cur-quadr-primes (quadr-prime-list a b)))
            (loop (incr-a a b b-max)
                  (incr-b b b-orig b-max)
                  (if (> (length cur-quadr-primes) (length (caddr lst)))
                      (list a b cur-quadr-primes)
                      lst)))))))

;; Run coefficients-and-max-consec-primes for the given problem parameters.
;; |a| <= 1000 and |b| <= 1000, which gives a range of -1000 <= a,b <= 1000.
(define problem-27
  (lambda ()
    (let ((quadr-prime-coeffs (coefficients-and-max-consec-primes -1000 1000 -1000 1000)))
      (* (car quadr-prime-coeffs) (cadr quadr-prime-coeffs)))))