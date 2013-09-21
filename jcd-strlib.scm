;;; Julian's string library, used for tackling Project Euler problems.

;; rotate will take the given string, and append its head to its tail.
;;
;; e.g., "abc" becomes "bca"
(define rotate
  (lambda (str)
    (if (zero? (string-length str))
        str
        (string-append (substring str 1 (string-length str)) 
                       (substring str 0 1)))))
