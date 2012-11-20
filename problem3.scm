;;; Project Euler, problem 3 - largest prime factor of a composite number.
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

;; Get the list of factors for a particular number.
(define factors
  (lambda (n)
    (begin
      (let ((n-upper (+ 1 (floor (sqrt n)))))
        (factor-list n n-upper)))))

;; Find all the prime factors of n, and sort from highest to lowest.
(define problem-5
  (lambda (n)
    (list-sort > (filter prime? (factors n)))))
