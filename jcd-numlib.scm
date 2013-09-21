;;; Julian's little numberic library, used for Project Euler problems.

(load "jcd-listlib.scm")
(load "jcd-strlib.scm")

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
          ((= n 4) #f)
          ((= n 5) #t)
          ((= n 6) #f)
          ((= n 7) #t)
          ((= n 8) #f)
          ((= n 9) #f)
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

;; n choose k
(define C
  (lambda (n k)
    (/ (! n) 
       (* (! k) 
          (! (- n k))))))
  
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

;;; Check to see if a given number is pandigital (contains all the digits
;;; from 1 to 9, and does not contain 0)
(define pandigital-lst?
  (lambda (num-lst digits-lst)
    (if (null? digits-lst)
        #t
        (if (and (zero? (occurrences num-lst 0)) (equal? (occurrences num-lst (car digits-lst)) 1))
            (pandigital-lst? num-lst (cdr digits-lst))
            #f))))
        
(define pandigital?
  (lambda (n)
    (let ((num-lst (number->list n)))
      (pandigital-lst? num-lst (num-list 9)))))

;; Find the nearest power of x to a given positive number.
(define nearest-power
  (lambda (n x)
    (let loop ((cur-pow 0)
               (prev 0))
      (let ((expt-val (expt x cur-pow)))
        (if (> expt-val n)
            prev
            (loop (+ cur-pow 1) expt-val))))))

;; Convert a decimal number into binary (but still as a decimal number, so
;; 11d gets converted into 1011d, which is (if it were actually binary) the
;; binary value.
(define decimal->binary
  (lambda (n)
    (string->number (decimal->binary-str n))))

;; Convert a decimal value into a binary value, stored as a string.
(define decimal->binary-str
  (lambda (n)
    (let ((x (nearest-power n 2)))
      (let loop ((cur-val n)
                 (cur-power x)
                 (binary-str (make-string 0)))
        (if (< cur-power 1)
            binary-str
            (loop ((lambda ()
                     (if (> cur-power cur-val)
                         cur-val
                         (- cur-val cur-power))))
                  (/ cur-power 2)
                  (string-append binary-str 
                                 ((lambda ()
                                    (if (> cur-power cur-val)
                                        "0"
                                        "1"))))))))))

;; closed form of sum from 1 to n
(define sum
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

;; tri-vpos returns the position in the 1D vector of the given row and column
;; of the triangle.
(define tri-vpos
  (lambda (row col)
    (+ (sum row) col)))

;; look up a particular position in the triangle.
(define tri-ref
  (lambda (tri row col)
    (vector-ref tri (tri-vpos row col))))

;; find-max-path-in-triangle performs dynamic programming, starting at
;; the bottom of a particular triangle, and working its way up, creating a
;; number of temporary maximum paths as it goes.
;;
;; An assumption is made that each row i of the triangle is length i.
(define find-max-path-in-triangle
  (lambda (triangle one-based-row)
    (let loop ((tri triangle)
               (row (- one-based-row 1)))
      (if (< row 0)
          (vector-ref tri 0)
          (loop (max-row-path tri row) 
                (- row 1))))))

;; max-row-path takes a triangle vector as a parameter, and a row q.
;; It iterates over each pair (0,1), (1,2), ... , (n-1, n) in the row.  For
;; each pair (i, i+1) in row q, it finds the maximum, and adds it to position 
;; i in row q-1.
(define max-row-path
  (lambda (tri row)
    (begin
      (let ((cur-tri (vector-copy tri)))
        (let loop ((i 0)
                   (ct cur-tri))
          ; If we're at the last element, we can't add any more, so
          ; return the current, updated triangle vector.
          (if (= i row)
              ct
              (begin
                (let ((update-idx (- (tri-vpos row i) row))
                      (elem-i (tri-ref ct row i))
                      (elem-i1 (tri-ref ct row (+ i 1))))
                  (vector-set! ct 
                               update-idx
                               (+ (vector-ref ct (- (tri-vpos row i) row))
                                  (max elem-i elem-i1)))
                  (loop (+ i 1) ct)))))))))

;; Get the list of proper factors of a number (does not include the number).
(define proper-divisors
  (lambda (n)
    (filter (lambda (q) 
              (not (= q n))) 
            (factors n))))

;; Get the list of factors for a particular number.
(define factors
  (lambda (n)
    (begin
      (let ((n-upper (floor (sqrt n))))
        (factor-list n n-upper)))))

;; Create a list of factors for a given number, checking "current" down to 0.
(define factor-list
  (lambda (n current)
    (if (= 0 current)
        '() ; base case - stop the recursion.
        (if (= 0 (modulo n current))
             ; If n mod current = 0, then we've got a factor.
             ; Add current, and n/current (since factors come in pairs),
             ; as long as n/current != current.
            (if (not (= current (/ n current)))
                (cons current (cons (/ n current) (factor-list n (- current 1))))
                (cons current (factor-list n (- current 1))))
            (factor-list n (- current 1))))))

;; divisor-sum-property returns 'abundant if the sum of a number's proper
;; divisors is greater than the number, 'perfect if it equals the number,
;; and 'deficient if it is less than the number.
(define divisor-sum-property
  (lambda (n)
    (let ((factor-sum (apply + (proper-divisors n))))
      (cond ((> factor-sum n) 'abundant)
            ((= factor-sum n) 'perfect)
            (else 'deficient)))))

;; abundant? returns #t if n is an abundant number, #f otherwise.
(define abundant?
  (lambda (n)
    (eq? 'abundant (divisor-sum-property n))))

;; perfect? returns #t if n is a perfect number, #f otherwise.
(define perfect?
  (lambda (n)
    (eq? 'perfect (divisor-sum-property n))))

;; deficient? returns #t if n is a deficient number, #f otherwise.
(define deficient?
  (lambda (n)
    (eq? 'deficient (divisor-sum-property n))))

;; sums returns all sums that can be created from the numbers in the list,
;; up to a given max.  The result is returned as a vector of size max,
;; where #t in a given position i means that i can be created by summing two
;; values in the list.
(define sums
  (lambda (lst max)
    (let ((sum-vec (make-vector max #f)))
      (let loop ((num-pairs (sym-pair-list lst)))
        (if (null? num-pairs)
            sum-vec
            (begin
              (let ((cur-sum (apply + (car num-pairs))))
                (if (< cur-sum max)
                    (vector-set! sum-vec (apply + (car num-pairs)) #t)))
              (loop (cdr num-pairs))))))))
            
;; rotations provides a list of all rotations of a given number, excluding the 
;; number itself.
(define rotations
  (lambda (num)
    (if (<= (string-length (number->string num)) 1)
        ; Single-digit numbers have no rotations that are not the same value.
        '()
        (let loop ((num-str (number->string num))
                   (initial-num #t)
                   (remain-rot (- (string-length (number->string num)) 1))
                   (rotation-lst '()))
          (if (zero? remain-rot)
              (cons (string->number num-str) rotation-lst)
              (loop (rotate num-str)
                    #f
                    (- remain-rot 1)
                    (if initial-num
                        rotation-lst 
                        (cons (string->number num-str) rotation-lst))))))))

;; checks to see if a number could be circular prime
;; first check: > 10? (anything under 10 is basically a special case)
;; second check: disallowed digits
(define potentially-circular-prime?
  (lambda (num)
    (if (< num 10)
        #t
        (let ((num-l (number->list num)))
          (not (or (member 2 num-l)
                   (member 4 num-l)
                   (member 5 num-l)
                   (member 6 num-l)
                   (member 8 num-l)
                   (member 0 num-l)))))))

;; returns #t if all the numbers in the rotation list are prime.
(define rotations-prime?
  (lambda (num)
    (let loop ((rotation-lst (rotations num)))
      (if (null? rotation-lst)
          #t
          (if (prime? (car rotation-lst))
              (loop (cdr rotation-lst))
              #f)))))
                      