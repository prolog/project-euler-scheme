;;; Project Euler, problem 29.  Number of distinct a^b values for
;;; lower <= a <= upper, and lower <= b <= upper.
;;;
;;; Copyright 2012 Julian Day <jcd748@mail.usask.ca>

;; Generate the list of the distinct values of the form a^b, given the
;; upper and lower bounds.
(define generate-integer-comb-list
  (lambda (lower-bound upper-bound)
    (let loop ((a lower-bound)
               (b lower-bound)
               (result-lst '()))
      (if (> a upper-bound)
          result-lst
          (let ((comb (expt a b)))
            (begin
              (if (not (member comb result-lst))
                  (set! result-lst (cons comb result-lst)))
              (if (>= b upper-bound)
                  (loop (+ a 1) lower-bound result-lst)
                  (loop a (+ b 1) result-lst))))))))

;; Get the length of the list of distinct values.
(define problem-29
  (lambda (lower-bound upper-bound)
    (length (generate-integer-comb-list lower-bound upper-bound))))