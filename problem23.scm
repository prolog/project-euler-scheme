;;; Project Euler, problem 23 - Non-abundant sums
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")
(load "jcd-listlib.scm")

;; By OEIS, A048242: 20161 is the last number that is not the sum of two
;; abundant numbers.
;;
;; So 20162 is the max to count up to.
(define na-max 20162)

;; not-abundant-sums returns all the numbers that cannot be expressed as the
;; sum of two abundant numbers.
(define not-abundant-sum
  (lambda (max)
    (let* ((abundant-numbers (filter abundant? (num-list max)))
           (abundant-sums-vec (sums abundant-numbers max)))
      (let loop ((i 1)
                 (non-abundant-sum-nums '()))
        (if (= i max)
            non-abundant-sum-nums
            (if (eq? #t (vector-ref abundant-sums-vec i))
                (loop (+ i 1) non-abundant-sum-nums)
                (loop (+ i 1) (cons i non-abundant-sum-nums))))))))


(define problem-23
  (lambda ()
    ; See OEIS A048242 for details.
    (apply + (not-abundant-sum na-max))))
                 