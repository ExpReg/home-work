#lang racket

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
                               ((symbol? (car lst)) (string-append " " ( symbol->string(car lst)) "=" (car (cdr lst)) (findAttribute (list-tail lst 2))))
                               (else (findAttribute (cdr lst))))))
(define body (tg "body"))
(define ul (tg "ul"))
(define li (tg "li"))
(define ol (tg "ol"))

(body
 (ul
  (map li (list "one" "two" "three"))
  )
 )

(body
    (let ((attributes (list 'start "3"))
          (contents   
               (map li (list "one" "two" "three"))))
       (ol 'id "demo" contents attributes)
    )
    )
 

