;;; Project Euler, problem 22 - Name scores
;;; Copyright 2013 Julian Day <jcd748@mail.usask.ca>

(load "jcd-listlib.scm")

;; word-score assumes that the "word" is passed in as a list of letters.
;; The word score return value is determined by summing the alphabetical
;; position of each letter.
(define word-score
  (lambda (word)
    (if (null? word)
        0
        (+ (+ 1 (char- (car word) #\A))
           (word-score (cdr word))))))

;; The list-score is determined by multiplying the word-score of each item in
;; the list by its index within the list, and summing the result.  It is
;; assumed that each item in the list is represented as a string.
(define list-score
  (lambda (lst idx)
    (if (null? lst)
        0
        (+ (* (word-score (string->list (car lst))) idx)
           (list-score (cdr lst) (+ idx 1))))))

;; Read in the list of names into a list of strings, sort them alphabetically,
;; and then get the list score.
(define problem-22
  (lambda (fname)
    (list-score (sort string<? (csvfile->list fname)) 1)))