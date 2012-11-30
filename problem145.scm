;;; Project Euler, problem 145 - reversible numbers
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Convert numeric chars into their number equivalents.
(define numchar->number
  (lambda (numchar)
    (cond ((eq? numchar #\0) 0)
          ((eq? numchar #\1) 1)
          ((eq? numchar #\2) 2)
          ((eq? numchar #\3) 3)
          ((eq? numchar #\4) 4)
          ((eq? numchar #\5) 5)
          ((eq? numchar #\6) 6)
          ((eq? numchar #\7) 7)
          ((eq? numchar #\8) 8)
          ((eq? numchar #\9) 9))))

;; Convert a number into a list of numbers.
(define number->list
  (lambda (n)
    (map numchar->number (string->list (number->string n)))))

;; Convert a list of numbers to a number.
;; e.g., (9 0 9 0) = 9090
(define list->number
  (lambda (lst)
    (if (null? lst)
        0
        (let loop ((num-lst lst)
                   (exp (- (length lst) 1)))
          (if (null? num-lst)
              0
              (+ (* (car num-lst) (expt 10 exp)) (loop (cdr num-lst) (- exp 1))))))))

;; A number is ineligible to be a reversible number if it has leading or
;; trailing 0s.
(define ineligible-reversible?
  (lambda (n)
    (let ((num-lst (number->list n)))
      (or (null? num-lst) (zero? (car num-lst)) (zero? (car (reverse num-lst)))))))

;; Given a list of numbers, check to see if they are all odd.
(define all-odd?
  (lambda (lst)
    (if (null? lst)
        #t
        (if (even? (car lst))
            #f
            (all-odd? (cdr lst))))))

;; Check to see if a given number is reversible.  A number is reversible if
;; n + (reverse n) consists entirely of odd digits.
(define reversible?
  (lambda (n)
    (if (ineligible-reversible? n)
        #f
        (let* ((n-lst (number->list n))
               (rev-n-lst (reverse n-lst))
               (rev-n (list->number rev-n-lst)))
          (if (> n rev-n)
              #f ; Don't double-count numbers!
              (all-odd? (number->list (+ n rev-n))))))))

;; Find how many numbers less than n are reversible by the definition in the
;; problem.
(define problem-145
  (lambda (n)
    (let loop ((i 1)
               (num-rev 0))
      (if (>= i n)
          num-rev
          (if (reversible? i)
              ;; Both the number and its reverse are reversible:
              (loop (+ i 1) (+ num-rev 2))
              (loop (+ i 1) num-rev))))))
      