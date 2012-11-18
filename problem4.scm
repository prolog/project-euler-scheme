;;; Project Euler, problem 4 - largest palindrome made from the product
;;; of two three-digit numbers.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Check to see if a given list is a palindrome.
(define palindrome?
  (lambda (lst)
    (equal? lst (reverse lst))))

;; Find the largest palindrome that is the product of three-digit numbers.
(define problem-4
  (lambda ()
    (let ((largest 0))
      (let loop ((i 999) (j 999))
        (let ((prod (* i j)))
           (if (and (<= i 99) (<= j 99))
              largest
              (begin
                (if (and (> prod largest) (palindrome? (string->list (number->string prod))))
                    (set! largest prod))
                (if (> j  1)
                    (loop i (- j 1))
                    (loop (- i 1) (- i 1))))))))))
	  