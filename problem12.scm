;;; Project Euler, problem 12 - triangle numbers,
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Create a list of numbers from 1 to n.
(define num-list
  (lambda (n)
    (if (<= n 0)
      '()
      (cons n (num-list (- n 1))))))

;; Get a particular triangle number.
;; The nth triangle number is 1 + 2 + ... + n.
(define triangle-number
  (lambda (n)
    (apply + (num-list n))))

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

;; Find the smallest triangle number with over n factors,
;; starting at triangle number (triangle-number i).
;; Print and die when done.
(define find-smallest-trinum-with-num-factors
  (lambda (i num-factors)
    (let ((t (triangle-number i)))
      (if (> (length (factors t)) num-factors)
	(begin
	  (display "Triangle number with > ")
	  (display num-factors)
	  (display " factors found: ")
	  (display t))
	(find-smallest-trinum-with-num-factors (+ i 1) num-factors)))))

