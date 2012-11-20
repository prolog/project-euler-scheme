;;; Project Euler, problem 7 - find the nth prime.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Check to see if a given number is prime.
(define prime?
  (lambda (n)
    (cond ((= n 1) #f)
          ((= n 2) #t)
          ((= n 3) #t)
           (else
            (if (zero? (modulo n 2))
                #f
                (let loop ((i 3))
                  (if (>= i n)
                      #t
                      (if (zero? (modulo n i))
                          #f
                          (loop (+ i 2))))))))))

;; Find all prime numbers up to a given n.
(define problem-7
  (lambda (n)
    (let ((lst '())
          (primes-found 0))
      (let loop ((i 1))
        (if (= n primes-found)
            (list-sort < lst) ; Found n primes - done!
            (if (prime? i)
                (begin
                  (set! lst (cons i lst))
                  (set! primes-found (+ primes-found 1))
                  (loop (+ i 1)))
                (loop (+ i 1))))))))