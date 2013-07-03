;;; Project Euler problem 17 - word count
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>
;;;
;;; Hack solution, only works up to 1000.
(define thousands
  '(("1" 11)))

(define hundreds
  '(("0" 0)
    ("1" 10)
    ("2" 10)
    ("3" 12)
    ("4" 11)
    ("5" 11)
    ("6" 10)
    ("7" 12)
    ("8" 12)
    ("9" 11)))

; Twenty, thirty, etc.
(define tens
  '(("2" 6)
    ("3" 6)
    ("4" 5)
    ("5" 5)
    ("6" 5)
    ("7" 7)
    ("8" 6)
    ("9" 6)))

(define and-word 3)

(define simple-letter-count
  (lambda (n)
    (cond ((equal? "01" n) 3)
          ((equal? "02" n) 3)
          ((equal? "03" n) 5)
          ((equal? "04" n) 4)
          ((equal? "05" n) 4)
          ((equal? "06" n) 3)
          ((equal? "07" n) 5)
          ((equal? "08" n) 5)
          ((equal? "09" n) 4)
          ((equal? "10" n) 3)
          ((equal? "11" n) 6)
          ((equal? "12" n) 6)
          ((equal? "13" n) 8)
          ((equal? "14" n) 8)
          ((equal? "15" n) 7)
          ((equal? "16" n) 7)
          ((equal? "17" n) 9)
          ((equal? "18" n) 8)
          ((equal? "19" n) 8)
          (else 0))))

(define get-ones-part
  (lambda (str)
    (if (zero? (string-length str))
        (make-string 0)
        (substring str (- (string-length str) 1) (string-length str)))))

(define get-tens-part
  (lambda (str)
    (if (<= (string-length str) 1)
        "0"
        (substring str (- (string-length str) 2) (- (string-length str) 1)))))

(define get-tens-and-ones
  (lambda (str len)
    (if (<= len 1)
        (string-append "0" (substring str 0 1))
        (substring str (- len 2) len))))

(define get-hundreds-part
  (lambda (str)
    (if (<= (string-length str) 2)
        (make-string 0)
        (substring str (- (string-length str) 3) (- (string-length str) 2))))) 

(define get-thousands-part
  (lambda (str)
    (if (<= (string-length str) 3)
        (make-string 0)
        (substring str (- (string-length str) 4) (- (string-length str) 3)))))

(define number-word-count
  (lambda (n)
    (let* ((str-num (number->string n))
           (str-num-len (string-length str-num))
           (thousands-part (get-thousands-part str-num))
           (thousands-length (string-length thousands-part))
           (hundreds-part (get-hundreds-part str-num))
           (hundreds-length (string-length hundreds-part))
           (tens-part (get-tens-part str-num))
           (ones-part (get-ones-part str-num))
           (tens-and-ones (get-tens-and-ones str-num str-num-len)))
            ; First case: 1000, 100, 200, etc.
      (cond ((and (equal? ones-part "0") (equal? tens-part "0"))
             (apply + (list (if (zero? thousands-length)
                                0
                                (cadr (assoc thousands-part thousands)))
                                (cadr (assoc hundreds-part hundreds)))))
            ; Second case: tens-part is 0 or 1
            ((or (equal? tens-part "0") (equal? tens-part "1"))
             (apply + (list (if (zero? thousands-length)
                                0
                                (cadr (assoc thousands-part thousands)))
                            (if (zero? hundreds-length)
                                0
                                (cadr (assoc hundreds-part hundreds)))
                            (if (not (zero? hundreds-length)) and-word 0)
                            (simple-letter-count tens-and-ones))))
            ; Third case: tens part is > 1
            (else (apply + (list (if (zero? thousands-length)
                                     0
                                     (cadr (assoc thousands-part thousands)))
                                 (if (zero? hundreds-length)
                                     0
                                     (cadr (assoc hundreds-part hundreds)))
                                 (if (not (zero? hundreds-length)) 
                                     and-word 
                                     0)
                                 (cadr (assoc tens-part tens))
                                 (simple-letter-count
                                  (string-append "0" ones-part)))))))))


      
(define problem-17
  (lambda (n)
    (let loop ((cnt 1)
               (sum 0))
      (if (> cnt n)
          sum
          (loop (+ cnt 1) 
                (+ sum (number-word-count cnt)))))))
