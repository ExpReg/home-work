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

(define timeEq? (lambda (time1 time2)
                  (if (null? time1) #t
                      (and (eq? (car time1) (car time2)) (timeEq? (cdr time1) (cdr time2))))))

(define getYear second)
(define getMonth third)
(define getDay fourth)
(define getHour fifth)
(define getMin sixth)

(define time (createTime 999 12 31 14 15))
(define time1 (createTime 2015 1 1 0 0))
(define time2 (createTime 2014 12 31 23 59))
(nrTime time)
(timeEq? time time1)

(< (nrTime time1) (nrTime time2))

;;; APPOINTMENT

(define createAppointment (lambda (text startTime endTime)
                            (if(or (not (string? text))(string? startTime) (string? endTime)) "ERROR:Somethings not right"
                            (list 'appointment text startTime endTime))))

(define app1 (createAppointment "haterKing" (createTime 2015 11 11 11 11) time))

(define appEq? (lambda (app1 app2)
                 (and (eq? (getText app1) (getText app2))
                      (timeEq? (getStartTime app1) (getStartTime app2))
                      (timeEq? (getEndTime app1 ) (getEndTime app2)))))


(define getText second)
(define getStartTime third)
(define getEndTime fourth)
(appEq? app1 (createAppointment "haterKing" (createTime 2015 11 11 11 11) time))  
;;;CALENDAR

(define createCalendar  (lambda r
                          (append (list 'calendar ) r)))

(define addToCal (lambda (calendar . r)
                   (append calendar r)))

;;tail recurvsei
(define removeFromCal (lambda (calendar . toRemove)
                        (removeFromCalH calendar toRemove '())))

;;Not DONE!!
(define removeFromCalH (lambda (calendar removeLst res)
                         res))
(define calendar1 (createCalendar app1 app1 app1))

(addToCal calendar1 app1)

