#lang racket
;; Generic functions

(define skipSymbol cdr)

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

(define table (tg "table"))
(define tr (tg "tr"))
(define td (tg "td"))
(define html(tg "html"))
(define body (tg "body"))
(define htmltable (tg "table"))
;;;;;;;;;
;;;
;;;
;;;TIME 
;;;
;;;
;;;;;;;;;
;Year: Cannot be lower than or equal zero
;Month: Cannot be bigger than 12 or lower than or equal 0 
;Day: Cannot be bigger than 31,lower than or equal 0
;Hour: Cannot be bigger than 24 
;Minutes (min) : cannot be lower than 0 or >= 60 
(define createTime (lambda (year month day hour min)
                     (let ((lst (list 'time year month day hour min)))
                     (cond ((not(checkYear? year)) "Error in year format")
                           ((not(checkMonth? month)) "Error in month format")
                           ((not(checkDay? day)) "Error in day format")
                           ((not(checkHour? hour)) "Error in hour format")
                           ((not(checkMin? min)) "Error in min format")
                           ((not(check-month-day? day month)) "Month do not support that many days")
                           (else lst)))))



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

(define check-month-day? (lambda (day month)
                          (cond ((and (<= month 7) (= (modulo month 2) 0)) (not (= day 31)))
                                ((and (> month 7 ) (= (modulo month 2) 1)) (not (= day 31)))
                                (else #t))))

;used when comparing times 
(define nrTime (lambda (time)
                 (+ (* (getYear time) (expt 10 8))  (* (getMonth time) (expt 10 6)) 
                    (* (getDay time)  (expt 10 4))  (* (getHour time) (expt 10 2)) (getMin time))))

(define timeEq? (lambda (time1 time2)
                  (if (null? time1) #t
                      (and (eq? (car time1) (car time2)) (timeEq? (cdr time1) (cdr time2))))))

;;Does not use hour or min
(define time->string(lambda (time)
                         (string-append (number->string (getDay time)) "-" (number->string (getMonth time)) "-" (number->string (getYear time)))))

(define next-date (lambda (time)
                    (let* ((month (getMonth time))
                          (day (getDay time))
                          (year(getYear time))
                          (hour (getHour time))
                          (min (getMin time))
                          (timeNextMonth (createTime year (+ 1 month) 1 hour min)))
                    (cond ((and (= month 12) (= day 31)) (createTime (+ 1 year) 1 1 hour min))
                          ((and (<= month 7) (= (modulo month 2) 0) (= day 30)) timeNextMonth)
                          ((and (<= month 7) (= (modulo month 2) 1) (= day 31)) timeNextMonth)
                          ((and (> month 7)  (= (modulo month 2) 1) (= day 30)) timeNextMonth)
                          ((and (> month 7)  (= (modulo month 2) 0) (= day 31)) timeNextMonth)
                          (else (createTime year month (+ 1 day) hour min))))))

(define next-week (lambda(time)
                    (next-weekH time 7)))

(define next-weekH (lambda(time counter)
                     (if (= counter 0) time
                         (next-weekH(next-date time) (- counter 1)))))
                        
(define generic-timeCheck (lambda (pred)
                            (lambda (time1 time2)
                              (pred (nrTime time1) (nrTime time2)))))

(define before (generic-timeCheck <))
(define after  (generic-timeCheck >))
(define same   (generic-timeCheck =))



(define getYear second)
(define getMonth third)
(define getDay fourth)
(define getHour fifth)
(define getMin sixth)
(define hour-min->string (lambda (time)
                           (string-append (number->string (getHour time)) ":" (number->string (getMin time)))))

(define time (createTime 999 12 31 14 15))
(define time1 (createTime 2015 1 1 0 0))
(define time2 (createTime 2014 12 31 23 59))

;;;;;;;;;
;;;
;;;
;;;APPOINTMENT
;;;
;;;;;;;;;

(define createAppointment (lambda (text startTime endTime)
                            (if(or (not (string? text))(string? startTime) (string? endTime)) "ERROR:Somethings not right"
                            (list 'appointment text startTime endTime))))

(define app1 (createAppointment "App1" (createTime 2015 11 11 11 11) (createTime 2015 11 11 12 30)))
(define app2 (createAppointment "App2" (createTime 2015 12 12 12 12) (createTime 2015 12 30 17 22)))

(define appEq? (lambda (app1 app2)
                 (and (eq? (getText app1) (getText app2))
                      (timeEq? (getStartTime app1) (getStartTime app2))
                      (timeEq? (getEndTime app1 ) (getEndTime app2)))))

(define isApp? (lambda(app)
                 (eq? (car app) 'appointment)))


(define appointment-overlap? (lambda (app1 app2)
                               (let* ((start1 (nrTime (getStartTime app1)))
                                     (start2 (nrTime (getStartTime app2)))
                                     (end1   (nrTime (getEndTime app1)))
                                     (end2   (nrTime (getEndTime app2)))
                                     (p      (< end1 start2))
                                     (q      (< end2 start1)))
                                 (not (and (or p q) (not (and p q))))))) ;; exclusive or 

(define getText second)
(define getStartTime third)
(define getEndTime fourth)


(define appointment-timeCheck (lambda (pred)
                               (lambda (app1 app2)
                                (pred (getStartTime app1) (getStartTime app2)))))

(define appointment-before (appointment-timeCheck before))
(define appointment-after (appointment-timeCheck after))
(define appointment-same (appointment-timeCheck same))

(define appointment->string (lambda (app)
                              (string-append (getText app) "(" (hour-min->string (getStartTime app)) "-" (hour-min->string(getEndTime app)) ")")))

;;;;;;;;;
;;;
;;;
;;;CALENDAR
;;;
;;;
;;;;;;;;;

(define createCalendar  (lambda (lst)
                          (cons 'calendar lst)))

(define addToCal (lambda (calendar . r)
                   (append calendar r)))

(define isCalendar? (lambda (cal)
                     (eq? (car cal) 'calendar)))

(define removeFromCal (lambda (cal . toRemove)
                        (removeFromCalH (skipSymbol cal) toRemove '())))


(define removeFromCalH (lambda (cal removeLst res)
                         (cond ((null? cal) (createCalendar (reverse res)))
                               ((and (isApp? (car cal)) (not(appIn (car cal) removeLst))) (removeFromCalH (cdr cal) removeLst (cons (car cal) res))) 
                               ((isCalendar? (car cal)) (removeFromCalH (cdr cal) removeLst 
                                                                        (cons (removeFromCalH (cadr cal) removeLst '()) res))) ;i use cadr to skip 'calendar
                               (else (removeFromCalH (cdr cal) removeLst res)))))



(define flatten-calendar (lambda (cal)
                           (createCalendar (flatten-calendarH (skipSymbol cal)))))
  
(define flatten-calendarH (lambda (cal)
                           (cond ((null? cal) '())
                                 ((isApp? (car cal)) (cons (car cal) (flatten-calendarH (cdr cal))))
                                 (else (append (flatten-calendarH (skipSymbol(car cal))) (flatten-calendarH (cdr cal)))))))

(define find-appointment (lambda (cal pred)
                           (let ((flat-cal (skipSymbol(flatten-calendar cal))))
                           (filter (lambda(x) (pred x)) flat-cal))))


(define generic-finder (lambda (version)
                         (lambda(cal pred)
                           (let((filtered-cal (find-appointment cal pred)))
                             (if(null? filtered-cal) #f
                                (generic-finderH filtered-cal version (car filtered-cal)))))))

(define generic-finderH (lambda (cal version app)
                          (cond ((null? cal) app)
                                ((version (car cal) app) (generic-finderH (cdr cal) version (car cal)))
                                (else (generic-finderH (cdr cal) version app)))))


(define find-first-appointment (generic-finder appointment-before))
(define find-last-appointment (generic-finder appointment-after))

(define calendars-overlap? (lambda (cal1 cal2)
                      (calendars-overlapH? (skipSymbol(flatten-calendar cal1)) (skipSymbol(flatten-calendar cal2)))))

(define calendars-overlapH? (lambda (cal1 cal2)
                             (cond ((null? cal1) #f)
                                   ((null?(find-appointment cal2 (lambda (x)(appointment-overlap? (car cal1) x))))(calendars-overlapH? (cdr cal1) cal2))
                                   (else #t))))
                                

(define appIn (lambda(x lst)
             (if(null? lst) #f
                (or (appEq? x (car lst)) (appIn x (cdr lst))))))
;;;;;;;;;
;;;
;;;
;;;Presentation stuff
;;;
;;;
;;;;;;;;;

;;calendars work on a weekly basis
(define present-calendar-html (lambda (cal from-time to-time)
                                  (present-calendar-html (skipSymbol (flatten-calendar cal)) from-time to-time)))


(define present-calendar-htmlH (lambda (cal from-time to-time)
                                 (html
                                  (body 
                                   (htmltable 'border "1" (create-rows cal from-time to-time))
                                   )
                                  )
                                 ))


(define create-rows (lambda (cal from-time to-time)
                      (create-rowsH cal from-time to-time 1)))

(define create-rowsH (lambda (cal from-time to-time counter)
                       (cond ((after from-time to-time) '())
                             (else (cons (list (tr (td (string-append "week:"(number->string counter)))) 
                                                   (tr (day-row from-time to-time)) (tr (app-row from-time to-time cal))) 
                                         (create-rowsH cal (next-week from-time) to-time (+ counter 1)))))))
                             
                                 
(define day-row (lambda (from-time to-time)
                      (day-rowH from-time to-time 7 '())))

(define day-rowH (lambda (from-time to-time counter res)
                   (if(or (= 0 counter) (after from-time to-time)) (reverse res)
                      (day-rowH (next-date from-time) to-time (- counter 1) (cons (td (time->string from-time)) res)))))

(define app-row (lambda (from-time to-time cal)
                  (app-rowH from-time to-time 7 '() cal)))

(define app-rowH (lambda (from-time to-time counter res cal)
                  (let* ((dayapp (createAppointment "" from-time (createTime (getYear from-time) (getMonth from-time) (getDay from-time) 23 59)))
                         (appstoday (find-appointment cal (lambda(x) (appointment-overlap? x dayapp)))))
                    (cond ((or (= counter 0)(after from-time to-time)) (reverse res))
                          ((null? appstoday) (app-rowH (next-date from-time) to-time (- counter 1) (cons (td "&nbsp;") res) cal))
                          (else (app-rowH (next-date from-time) to-time (- counter 1) 
                                          (cons (td (apply string-append(map appointment->string appstoday))) res) cal))))))
                          


                                 
;;;;DEFS FOR TESTING
(define calendar1 (createCalendar (list app1 app2)))
(define calendar2 (createCalendar (list calendar1 app1 app2)))
(define calendar3 (createCalendar (list (createAppointment "hejsa" (createTime 2012 11 11 11 11) (createTime 2012 11 11 11 11)))))
(removeFromCal calendar1 app1 app2)
(removeFromCal calendar1 app1)
(removeFromCal calendar1 app2)

(find-last-appointment calendar1 (lambda(x) #t))




