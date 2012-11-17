;;; Project Euler, problem 25 - first Fibonacci number to contain 1000
;;; digits

;;; "Mostly iterative" fibonacci (tail recursion)
(define fibonacci
  (lambda (n)
    (let loop ((fib-term 1)
	       (prev 0)
	       (cur 1))
      (if (= fib-term n)
	cur
	(loop (+ fib-term 1) cur (+ prev cur))))))

;; Find the Fibonacci number of the given length
(define find-fib-of-length
  (lambda (cur-fib len)
    (if (= len (length (string->list (number->string (fibonacci cur-fib)))))
      cur-fib
      (find-fib-of-length (+ cur-fib 1) len))))

;; Use (problem-25 1000) to solve
(define problem-25
  (lambda (num-digits)
    (find-fib-of-length 1 num-digits)))

