;; Julian's mini list library.

;; Convert a list of numbers to a number.
;; e.g., (9 0 9 0) = 9090
(define list->number
  (lambda (lst)
    (if (null? lst)
        0
        (let loop ((num-lst lst)
                   (exp (- (length lst) 1)))
          (if (null? num-lst)
              0
              (+ (* (car num-lst) (expt 10 exp)) (loop (cdr num-lst) (- exp 1))))))))

(define insert
  (lambda (l n e)
    (if (= 0 n)
        (cons e l)
        (cons (car l)
              (insert (cdr l) (- n 1) e)))))
 
(define sequence 
  (lambda (start end)
    (if (= start end)
        (list end)
        (cons start (sequence (+ start 1) end)))))
 
(define permute
  (lambda (lst)
    (if (null? lst)
        '(())
        (apply append (map (lambda (p)
                             (map (lambda (n)
                                    (insert p n (car lst)))
                                  (sequence 0 (length p))))
                           (permute (cdr lst)))))))

;; Get the number of occurrences of an item within a list
(define occurrences
  (lambda (lst item)
    (let loop ((occ-lst lst)
               (occ 0))
      (if (null? occ-lst)
          occ
          (if (equal? (car occ-lst) item)
              (loop (cdr occ-lst)(+ occ 1))
              (loop (cdr occ-lst) occ))))))