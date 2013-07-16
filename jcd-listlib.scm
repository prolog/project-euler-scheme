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

;; Reads a file into a list.
(define file->list
  (lambda (file)
    (let ((port (open-input-file file)))
      (let file-loop ((x (read port)))
        (if (eof-object? x)
            (begin
              (close-input-port port)
              '())
            (cons x (file-loop (read port))))))))

;; Read a CSV file into a list.  Assumption is that everything between
;; commas is quoted.
(define csvfile->list
  (lambda (filename)
    (let ((lst '())
          (file (open-input-file filename)))
      (let loop ((cur-val (read file))
                 (llst lst))
        (if (eof-object? cur-val)
            llst
            ; Check to see if the first character is a #\,
            ; Everything after the first element should be.
            (if (pair? cur-val)
                (loop (read file) (append llst (cdr cur-val)))
                ; If it's not a pair, it'll be the first item
                ; read, which will be a string.
                (loop (read file) (cons cur-val '()))))))))

;; Creates a list of lists, where the first item of each sublist is the head
;; of the original list, and the second item is the current head.
;;
;; '(1) produces '((1 1))
;; '(1 2 3 4 5) produces '((1 5) (1 4) (1 3) (1 2) (1 1))
(define pairs-list
  (lambda (lst)
    (if (null? lst)
        lst
        (let loop ((head (car lst))
                   (rest (cdr lst))
                   (cur-pairs (list (list (car lst) (car lst)))))
          (if (null? rest)
              cur-pairs
              (loop head (cdr rest) (cons (list head (car rest)) cur-pairs)))))))

;; Given a list of numbers '(a b c d e), sym-pair-list returns a list of pairs
;; in the form '((a b) (a c) (a d) (a e) (b c) ...)
(define sym-pair-list
  (lambda (lst)
    (if (or (null? lst) (= 0 (length lst)))
        lst
        (let loop ((sublist lst)
                   (cur-pairs '()))
          (if (<= (length sublist) 0)
              cur-pairs
              (begin
                ; Flatten the results of pairs-list into the
                ; current structure.
                (for-each (lambda (x) 
                            (set! cur-pairs (cons x cur-pairs))) 
                          (pairs-list sublist))
                (loop (cdr sublist) cur-pairs)))))))