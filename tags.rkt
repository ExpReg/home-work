#lang racket
;; Generic functions



;; HTML STUFF 
(define tg (lambda (head) 
             (lambda r
                 (string-append "<" head (findAttribute r)">" (findContent r) "</" head ">"))))

(define findContent (lambda (lst)
                      (cond ((null? lst) "")
                            ((string? (car lst)) (string-append (car lst) (findContent (cdr lst))))
                            ((procedure? (car lst)) (string-append ((car lst)) (findContent (cdr lst))))
                            ((list? (car lst)) (string-append (findContent (car lst)) (findContent (cdr lst))))
                            (else (findContent (list-tail lst 2))))))
                        
(define findAttribute (lambda (lst)
                        (cond ((null? lst) "")
                               ((symbol? (car lst)) (string-append " " ( symbol->string(car lst)) "=" (cadr lst) (findAttribute (list-tail lst 2))))
                               (else (findAttribute (cdr lst))))))

;;TIME 
;Year: Cannot be lower than or equal zero
;Month: Cannot be bigger than 12 or lower than or equal 0 
;Day: Cannot be bigger than 31,lower than or equal 0
;Hour: Cannot be bigger than 24 
;Minutes (min) : cannot be lower than 0 or >= 60 
(define createTime (lambda (year month day hour min)
                     (let ((lst (list 'time year month day hour min)))
                     (if (and (checkYear? year) (checkMonth? month) (checkDay? day) (checkHour? hour) (checkMin? min)) lst
                        "Error: PLEASE INSERT CORRECT TIME FORMAT"))))


(define checkYear?  (lambda (year)
                     (>= year 0)))

(define checkMonth? (lambda (month)
                     (and (> month 0) (<= month 12))))

(define checkDay?  (lambda (day) 
                    (and (<= day 31) (>= day 1))))

(define checkHour?  (lambda (hour)
                     (and (>= hour 0) (<= hour 23))))

(define checkMin?   (lambda (min) 
                      (and (>= min 0 ) (< min 60 ))))

;used when comparing times 
(define nrTime (lambda (time)
                 (+ (* (getYear time) (expt 10 8))  (* (getMonth time) (expt 10 6)) 
                    (* (getDay time)  (expt 10 4))  (* (getHour time) (expt 10 2)) (getMin time))))

(define getYear second)
(define getMonth third)
(define getDay fourth)
(define getHour fifth)
(define getMin sixth)

(define time (createTime 999 12 31 14 15))
(define time1 (createTime 2015 1 1 0 0))
(define time2 (createTime 2014 12 31 23 59))
(nrTime time)

(< (nrTime time1) (nrTime time2))

;;; APPOINTMENT

(define createAppointment (lambda (text startTime endTime)
                            (if(or (not (string? text))(string? startTime) (string? endTime)) "ERROR:Somethings not right"
                            (list text startTime endTime))))

(define app1 (createAppointment "haterKing" (createTime -1 11 11 11 11) time))
  
;;;CALENDAR 


(define createCalendar  (lambda r
                          (append (list 'calendar ) r)))
