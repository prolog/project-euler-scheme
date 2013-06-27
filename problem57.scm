;;; Project Euler, problem 57 - investigate expansions of the continued
;;; fraction of sqrt 2.
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

;; pell-lucas-relation calculates a recurrence relation as follows:
;;
;; i, if n = 0
;; j, if n = 1
;; 2(P_(n-1)) + P_(n-2) otherwise.
;;
;; The above can be used to calculate Pell numbers (use i=0, j=1)
;; or Pell-Lucas numbers (use i = j = 2)
(define pell-lucas-relation
  (lambda (i j n)
    (cond ((<= n 0) (list i))
          ((= n 1) (list j i))
          (else
            (let loop ((k 2)
                       (pell-nums (list j i)))
              (if (> k n)
                  pell-nums
                  (loop (+ k 1)
                        (cons (+ (* 2 (car pell-nums))
                                 (cadr pell-nums))
                              pell-nums))))))))

;; Calculate up to the nth Pell number.
;;
;; Pell numbers are defined by the recurrence relation:
;;
;; 0, if n = 0
;; 1, if n = 1
;; 2(P_(n-1)) + P_(n-2) otherwise
;;
;; pell-numbers returns a reverse list: the first number is the nth
;; Pell number, the second is the (n-1)th, etc.  So reverse the list
;; to return them in order.
(define pell-numbers
  (lambda (n)
    (reverse (pell-lucas-relation 0 1 n))))
      
;; The numerator of the approximation of sqrt 2 is the sum of a Pell number
;; and its predecessor.
(define pell-numerators
  (lambda (n)
    (let loop ((pell-nums (pell-numbers n))
               (numerator-lst '(0)))
      (cond ((null? pell-nums) (reverse numerator-lst))
            ((= 1 (length pell-nums)) (reverse numerator-lst))
            (else
             (loop (cdr pell-nums)
                   (cons (+ (car pell-nums)
                            (cadr pell-nums))
                         numerator-lst)))))))

;; Check to see if the numerator has more digits than the
;; denominator, by converting to a string and then just
;; checking length.
(define n-more-digits-than-d
  (lambda (n d)
    (let ((ns (number->string n))
          (ds (number->string d)))
      (> (string-length ns) (string-length ds)))))

;; Starting with the first expansion defined by the Project Euler
;; website (1 + 1/2 = 3/2 = 1.5), loop through the first thousand
;; expansions and find the number of expansions where the number of
;; digits in the numerator exceeds the number of digits in the
;; denominator.
(define problem-57
  (lambda (num)
    (let ((n (pell-numerators (+ num 2)))
          (d (pell-numbers  (+ num 2))))
      (let loop ((numerators n)
                 (denominators d)
                 (count 0))
        (if (or (null? numerators) (null? denominators))
            count
              (loop (cdr numerators)
                    (cdr denominators)
                    (if (n-more-digits-than-d (car numerators) 
                                              (car denominators))
                        (+ count 1)
                        count))))))) 