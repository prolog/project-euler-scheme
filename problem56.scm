;;; Project Euler, problem 56 - maximum digital sum
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

(define problem-56
  (lambda (upper-bound)
    (let loop ((a 1)
               (b 1)
               (max-digital-sum 0))
      (if (> a upper-bound)
          max-digital-sum
          (let ((digital-sum (apply + (number->list (expt a b)))))
            (begin
              (if (> digital-sum max-digital-sum)
                  (set! max-digital-sum digital-sum))
              (if (> b upper-bound)
                  (loop (+ a 1) 1 max-digital-sum)
                  (loop a (+ b 1) max-digital-sum))))))))