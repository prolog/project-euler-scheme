;;; Project Euler problem 14 - Collatz Problem
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

(define collatz
  (lambda (n)
    (if (even? n)
        (/ n 2)
        (+ (* 3 n) 1))))

(define collatz-sequence
  (lambda (n)
    (if (= n 1)
        '()
        (cons (collatz n) (collatz-sequence (collatz n))))))

(define create-collatz-sequence
  (lambda (n)
    (cons n (collatz-sequence n))))

(define problem-14
  (lambda (n)
    (let ((largest (cons 0 '())))
      (let loop ((i 1))
        (if (>= i n)
            (car largest)
            (let ((collatz-i (create-collatz-sequence i)))
              (begin
                (if (> (length collatz-i) (length (cdr largest)))
                    (set! largest (cons i collatz-i)))
                (loop (+ i 1)))))))))