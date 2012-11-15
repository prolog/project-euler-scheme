(define mult-3-or-5?
  (lambda (n)
    (if (or
	  (= 0 (modulo n 3))
	  (= 0 (modulo n 5))
	)
    #t
    #f)))

(define num-list
  (lambda (n)
    (if (<= n 0)
      '()
      (cons n (num-list (- n 1))))))


; Find the sum of all natural numbers < 1000 that are multiples of 3 or 5.
(apply + (filter mult-3-or-5? (num-list 999)))
