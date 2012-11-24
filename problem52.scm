;;; Project Euler, problem 52
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Convert a char to a number.  I'm sure there's a better way to do this...
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

;; Check to see if all elements in a given list are equal
(define all-equal?
  (lambda (element lst)
    (if (null? lst)
        (equal? element lst)
        (if (equal? element (car lst))
            (if (not (null? (cdr lst)))
                (all-equal? element (cdr lst))
                #t)
            #f))))
        

;; Map a function over a list of lists
(define list-map
  (lambda (fn lst)
    (if (null? lst)
        '()
        (cons (map fn (car lst)) (list-map fn (cdr lst))))))

;; Sort each list contained within the list
(define recsort
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list-sort < (car lst)) (recsort (cdr lst))))))

;; Check to see if the given multiples of i contain the same digits
(define multiples-contain-same-digits?
  (lambda (i mult-lst)
    (let ((result-lst (recsort (list-map numchar->number (map string->list (map number->string (map (lambda (j) (* i j)) mult-lst)))))))
      (all-equal? (car result-lst) result-lst))))

;; Find the smallest positive integer such that its multiples from 2 through
;; 6 (also inclusive) contain exactly the same digits (order will obviously
;; be different).
(define problem-52
  (lambda ()
    (let loop ((i 1))
      (if (not (multiples-contain-same-digits? i '(2 3 4 5 6)))
          (loop (+ i 1))
          i))))
