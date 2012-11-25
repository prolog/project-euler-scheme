;;; Project Euler, problem 21.
;;;
;;; Find the sum of all amicable numbers less than 10000.


;; Create a list of numbers from 1 to n.
(define num-list
  (lambda (n)
    (if (<= n 0)
      '()
      (cons n (num-list (- n 1))))))

;; Create a list of factors for a given number, checking "current" 
;; down to 0.
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

;; Get the list of proper factors for a particular number (don't include that
;; particular number in the factor list).
(define proper-factors
  (lambda (n)
    (begin
      (filter (lambda (x)
                (not (= n x)))
              (factor-list n (+ 1 (floor (sqrt n))))))))

(define d
  (lambda (n)
    (apply + (proper-factors n))))

(define amicable-list
  (lambda (limit)
    (let ((lst '()))
      (let loop ((i 2))
        (if (>= i limit)
            lst
            (begin
              (let* ((da (d i))
                     (db (d da)))
                (if (and (= i db) (not (= i da)))
                    (set! lst (cons da lst))))
              (loop (+ i 1))))))))
          
(define problem-21
  (lambda (limit)
    (apply + (amicable-list limit))))