;;; Project Euler, problem 40 - Champernowe's constant

(load "jcd-numlib.scm")

;; Check to see if the digit we're currently searching for is somewhere
;; within digit-cnt and digit-cnt + length-of-count.
(define within-range
  (lambda (digit-cnt count current-digit-to-find)
    (and (>= current-digit-to-find digit-cnt) (> (+ digit-cnt (string-length (number->string count))) current-digit-to-find))))

;; Get a particular digit from a number.
(define digit-at
  (lambda (number pos)
    (numchar->number (string-ref (number->string number) pos))))

;; Get the (ith, (i+1)th, ...) digits from a number formed by concatenating
;; all the integers.  Stop counting at a given n.
(define champernowe
  (lambda (n digits)
    (let loop ((count 1) 
               (digit-cnt 1) 
               (cur-digits digits) 
               (current-digit (car digits)) 
               (champernowe-digits '()))
      (if (or (> digit-cnt n) (null? cur-digits))
          champernowe-digits
          (let ((in-range (within-range digit-cnt count current-digit)))
            (loop (+ count 1)
                  (+ digit-cnt (string-length (number->string count)))
                  (if in-range
                      (cdr cur-digits)
                      cur-digits)
                  (if in-range
                      (if (not (null? (cdr cur-digits)))
                          (cadr cur-digits)
                          (cdr cur-digits))
                      current-digit)
                  (if in-range
                      (cons (digit-at count (- current-digit digit-cnt)) champernowe-digits)
                      champernowe-digits)))))))

;; Get the product of the given digits of Champernowe's constant.
(define problem-40
  (lambda (n digits)
    (apply * (champernowe n digits))))