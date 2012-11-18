;;; Project Euler, problem 9 - Pythagorean triplets.
;;;
;;; Copyright (c) 2012 Julian Day <jcd748@mail.usask.ca>

;; Compute c^2 from a^2 and b^2
(define pythag
  (lambda (a b)
    (+ (* a a) (* b b))))

;; Find Pythagorean triplets that match the given parameter.
;; Call (problem-9 1000) to solve.
(define problem-9
  (lambda (n)
    (let loop ((i (/ n 2)) (j (/ n 2)))
      (if (or (zero? i) (zero? j))
          (display "Nothing found.")
          (let ((k (sqrt (pythag i j))))
            (if (= n (+ i j k))
                (list i j k)
                (if (> j 1)
                    (loop i (- j 1))
                    (loop (- i 1) (- i 1)))))))))
