;;; Julian's little numberic library, used for Project Euler problems.

;; Create a list of numbers from 1 to n.
(define num-list
  (lambda (n)
    (if (<= n 0)
      '()
      (cons n (num-list (- n 1))))))

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

;; Mark all multiples of a given number to be composite, up to the length
;; of the composites vector.
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

;; Factorial
(define !
  (lambda (n)
    (let loop ((i n)
               (result 1))
      (if (zero? i)
          result
          (loop (- i 1) (* result i))))))
  
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

;; Convert a number into a list of numbers equal to the digits of the
;; original number.
(define number->list
  (lambda (num)
    (map numchar->number (string->list (number->string num)))))

;;; Sum of powers: if given a number like 1234 and a power 4, sum
;;; 1^4 + 2^4 + 3^4 + 4^4
(define sum-of-powers
  (lambda (n pow)
    (let ((num-lst (number->list n)))
      (apply + (map (lambda (x)
                      (expt x pow)) num-lst)))))