;;; Project Euler, problem 18 - Maximum Path Sum I
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-numlib.scm")

(define problem-18-triangle '#(
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23))

;; tri-vpos returns the position in the 1D vector of the given row and column
;; of the triangle.
(define tri-vpos
  (lambda (row col)
    (+ (sum row) col)))

;; look up a particular position in the triangle.
(define tri-ref
  (lambda (tri row col)
    (vector-ref tri (tri-vpos row col))))

;; find-max-path-in-triangle performs dynamic programming, starting at
;; the bottom of a particular triangle, and working its way up, creating a
;; number of temporary maximum paths as it goes.
;;
;; An assumption is made that each row i of the triangle is length i.
(define find-max-path-in-triangle
  (lambda (triangle one-based-row)
    (let loop ((tri triangle)
               (row (- one-based-row 1)))
      (if (< row 0)
          (vector-ref tri 0)
          (loop (max-row-path tri row) 
                (- row 1))))))

;; max-row-path takes a triangle vector as a parameter, and a row q.
;; It iterates over each pair (0,1), (1,2), ... , (n-1, n) in the row.  For
;; each pair (i, i+1) in row q, it finds the maximum, and adds it to position 
;; i in row q-1.
(define max-row-path
  (lambda (tri row)
    (begin
      (let ((cur-tri (vector-copy tri)))
        (let loop ((i 0)
                   (ct cur-tri))
          ; If we're at the last element, we can't add any more, so
          ; return the current, updated triangle vector.
          (if (= i row)
              ct
              (begin
                (let ((update-idx (- (tri-vpos row i) row))
                      (elem-i (tri-ref ct row i))
                      (elem-i1 (tri-ref ct row (+ i 1))))
                  (vector-set! ct 
                               update-idx
                               (+ (vector-ref ct (- (tri-vpos row i) row))
                                  (max elem-i elem-i1)))
                  (loop (+ i 1) ct)))))))))

(define problem-18
  (lambda ()
    (find-max-path-in-triangle problem-18-triangle 15)))