;;; Project Euler, problem 24 - lexicographic permutations
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-listlib.scm")

;; Given a list of digits to permute, find the nth value.  This is done
;; very simply, by just generating all permutations, converting these to
;; a number, sorting the list, converting this to a vector, and then getting
;; the appropriate item.
(define problem-24
  (lambda (perm-values n)
    (let ((sorted-perm-vec (list->vector (sort < (map list->number (permute perm-values))))))
      (vector-ref sorted-perm-vec (- n 1)))))