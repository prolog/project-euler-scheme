;;; Project Euler, problem 19 - Counting Sundays.
;;;
;;; Copyright (c) 2013 Julian Day <jcd748@mail.usask.ca>

;; The generated dates have the format '(day-of-week month day-of-month year)

;; 20th-century? returns true if the given date falls between january 1
;; 1901 and december 31 2000.
(define date-in-century?
  (lambda (date century)
    (let* ((last-year (* century 100))
           (first-year (- last-year 99))
           (date-year (cadddr date)))
      (and (>= date-year first-year)
           (<= date-year last-year)))))

;; is-day-first-of-month-in-20c? returns #t if the current date is the 1st of 
;; a month in a year in the 20th century, and the current day-of-week is a
;; sunday.
(define sunday-first-of-month-in-20c?
  (lambda (date)
    (let ((day (caddr date))
          (day-of-week (car date))
          (year (cadddr date)))
      (and (equal? day 1)
           (equal? day-of-week 'sunday)
           (date-in-century? date 20)))))

;; incr-day-of-week returns the next day of the week
(define incr-day-of-week
  (lambda (day)
    (cond ((equal? day 'monday) 'tuesday)
          ((equal? day 'tuesday) 'wednesday)
          ((equal? day 'wednesday) 'thursday)
          ((equal? day 'thursday) 'friday)
          ((equal? day 'friday) 'saturday)
          ((equal? day 'saturday) 'sunday)
          ((equal? day 'sunday) 'monday)
          (else #f))))

;; incr-month returns the next month
(define incr-month
  (lambda (month)
    (cond ((equal? month 'january) 'february)
          ((equal? month 'february) 'march)
          ((equal? month 'march) 'april)
          ((equal? month 'april) 'may)
          ((equal? month 'may) 'june)
          ((equal? month 'june) 'july)
          ((equal? month 'july) 'august)
          ((equal? month 'august) 'september)
          ((equal? month 'september) 'october)
          ((equal? month 'october) 'november)
          ((equal? month 'november) 'december)
          ((equal? month 'december) 'january)
          (else #f))))

;; incr-year returns the next year
(define incr-year
  (lambda (year)
    (+ year 1)))

;; incr-day increments the day of the month, or resets it to 1 if the day
;; is the last day of the given month.
(define incr-day
  (lambda (date)
    (let ((day (caddr date))
          (month (cadr date))
          (year (cadddr date)))
      (if (last-day-of-month? day month year)
          1
          (+ day 1)))))

;; last-day-of-month? returns #t if the given day is the last in the month,
;; #f otherwise.
;; 
;; The rhyme goes:
;;
;; Thirty days has September,
;; April, June and November.
;; All the rest have thirty-one,
;; Saving February alone,
;; Which has twenty-eight, rain or shine.
;; And on leap years, twenty-nine.
(define last-day-of-month?
  (lambda (day month year)
    (cond ((or (equal? month 'september)
               (equal? month 'april)
               (equal? month 'june)
               (equal? month 'november)) (equal? day 30))
          ((equal? month 'february)
               (equal? day (if (leap-year? year)
                               29
                               28)))
          (else (equal? day 31)))))

;; Checks to see if the current year is a leap year.  A simplified
;; specification is used, where a leap year is divisible by 4, but not
;; by 100 unless also divisible by 400.
(define leap-year?
  (lambda (year)
    (and (zero? (mod year 4))
         (or (not (zero? (mod year 100)))
             (zero? (mod year 400))))))
    

;; Increments a given date
(define incr-date
  (lambda (date)
    (let* ((month (cadr date))
           (day (caddr date))
           (year (cadddr date))
           (is-last-day (last-day-of-month? day month year)))
      (list (incr-day-of-week (car date))
            (if is-last-day
                (incr-month month)
                month)
            (incr-day date)
            (if (and is-last-day
                     (equal? month 'december))
                (incr-year year)
                year)))))

;; fs-date-equal? compares a full date (day-of-week month day-of-month year) 
;; to a short date (month day of month year)
(define fs-date-equal?
  (lambda (full short)
    (equal? (cdr full) short)))

;; dates-in-range returns a list of all dates in the range of end to
;; start, inclusive.  start must be a full date (include day of week),
;; while end is a short date, having only month, day-of-month, and year.
(define dates-in-range
  (lambda (start end)
    (let loop ((current start)
               (all-dates '()))
      (if (fs-date-equal? current end)
          all-dates
          (loop (incr-date current) (cons current all-dates)))))) 

;; start is a full date, and end is a short date, as defined for procedure
;; dates-in-range.
(define problem-19
  (lambda (start end)
    (length (filter sunday-first-of-month-in-20c? (dates-in-range start end)))))