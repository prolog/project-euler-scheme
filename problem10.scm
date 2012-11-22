;;; Project Euler, problem 10 - find the sum of all primes below n.
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
                (let ((sqrtn (sqrt n)))
                  (let loop ((i 3))
                    (if (>= i (sqrt n))
                        #t
                        (if (zero? (modulo n i))
                            #f
                            (loop (+ i 2)))))))))))

(define sieve
  (lambda (vec idx)
    (let loop ((i 2)) ; = '2, since vectors begin at 0.
      (if (>= (* i idx) (vector-length vec))
          #t
          (begin
            (vector-set! vec (* idx i) #t)
            (loop (+ i 1)))))))

;; Find all primes less than n, using the Sieve of Eratosthenes.
(define eratosthenes
  (lambda (n)
    (let ((prime-lst '())
          (composite-numbers (make-vector n #f)))
      (let loop ((i 2))
        (if (>= i n)
            prime-lst ; Found the primes below n
            (begin
              (if (not (vector-ref composite-numbers i))
                  (if (prime? i)
                      (begin
                        (sieve composite-numbers i)
                        (set! prime-lst (cons i prime-lst)))))
              (loop (+ i 1))))))))

;; Find the sum of all primes less than n
(define problem-10
  (lambda (n)
    (apply + (eratosthenes n))))
