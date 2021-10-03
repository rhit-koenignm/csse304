; Natalie Koenig
; CSSE304-03
; Assignment 10
; Prof: Buffalo

; Assignment 10
; worked with Nick Bohner

; #1 
; free-vars
; (define free-vars 
;   (lambda (exp)
;     (cond 
;       [(symbol? exp )])
;   ))
(define free-vars 
  (lambda (exp)
    (cond [(null? exp) '()]
          [(symbol? exp) (list exp)]
          [(eqv? 'lambda (car exp)) 
            (filter (lambda (x) (not (contains? x (cadr exp)))) (free-vars (caddr exp)))]
          [else (union (free-vars (car exp)) (free-vars (cdr exp)))])))

; bound-vars
(define bound-vars
  (lambda (exp)
    (cond [(null? exp) '()]
          [(symbol? exp) '()]
          [(eqv? 'lambda (car exp)) 
            (filter (lambda (x) (contains? x (cadr exp))) (union (free-vars (caddr exp)) (bound-vars (caddr exp))))]
          [else (union (bound-vars (car exp)) (bound-vars (cdr exp)))])))

; #2 

(define occurs-free?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      [(symbol? exp) (eqv? var exp)]
      [(eqv? (car exp) 'lambda) 
       (and (not (contains? var (cadr exp)))
            (occurs-free? var (caddr exp)))]
      [(eqv? (car exp) 'let)
        (occurs-free? var (let->application exp))]
      [(eqv? (car exp) 'let*)
        (occurs-free? var (let*->let exp))]
      [(eqv? (car exp) 'set!) 
        (and (not (equal? var (cadr exp)))
          (not (eqv? var 'set!)))]        
      [else (or (occurs-free? var (car exp))
                (occurs-free? var (cdr exp)))])))

(define occurs-bound?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      [(symbol? exp) #f]
      [(eqv? (car exp) 'lambda) 
       (or (occurs-bound? var (caddr exp))
           (and (contains? var (cadr exp)) 
                (occurs-free? var (caddr exp))))]
      [(eqv? (car exp) 'let)
        (occurs-bound? var (let->application exp))]
      [(eqv? (car exp) 'let*)
        (occurs-bound? var (let*->let exp))]   
      [else (or (occurs-bound? var (car exp))
                (occurs-bound? var (cdr exp)))])))



(define (let->application lst)
  (append (list (list 'lambda (map car (cadr lst)) (caddr lst))) (map cadr (cadr lst))))  

(define (let*->let lst)
  (cond [(equal? (length (cadr lst)) 1) (append (list 'let) (cdr lst))]
        [else (append (list 'let) (list (list (caadr lst))) (list (let*->let (append (list 'let) (list (cdadr lst)) (cddr lst)))))]))

(define lexical-address
  (lambda (exp)
    (cond [(null? exp) '()]
          [(symbol? exp) (list ': 'free exp)]
          [else (cons (lexical-address (car exp)) (lexical-address (cadr exp)))])))

; (define lexical-address-helper
;   (lambda (var scope-list)
;     (cond [(null? exp) '()]
;           [(symbol? exp) (cons '')]
;           [(eqv? ())]
;           [else (list (lexical-address (car exp)) (lexical-address (cadr exp)))])))

; (define lexical-address-helper
;   (lambda (scope-list)))

(define un-lexical-address
  (lambda (lex-address)
    (cond [(null? lex-address) '()]
          [(symbol? (car (lex-address)))] 
          [else (cons (un-lexical-address (car lex-address)) (un-lexical-address (cdr lex-address)))])))


; Recursive helper methods
; (define notate-depth-in-address)

(define notate-depth 
  (lambda (slist)
    (notate-depth-in-s-list slist 0)))

(define notate-depth-in-s-list
  (lambda (slist d)                     
    (if (null? slist) ;\new5
      '()
      (cons
        (notate-depth-in-symbol-expression (car slist) d)  
        (notate-depth-in-s-list (cdr slist) d)))))

(define notate-depth-in-symbol-expression
  (lambda (se d)
    (if (symbol? se)
      (list se d)
      (notate-depth-in-s-list se (+ d 1)))))


; Recursive function that appends all unique elements from set 2 into set 1
(define (intersection s1 s2)
  (cond [(not (set? s2)) '()]
        [(null? s2) '()]
        [(null? s2) '()]
        [(null? s1) '()]
        [(contains? (car s2) s1) (append (list (car s2)) (intersection s1 (cdr s2)))]
        [else (intersection s1 (cdr s2))]))

; Recursive function that appends all unique elements from set 2 into set 1
(define (union set1 set2)
  (cond [(null? set2) set1]
        [(null? set1) set2]
        [(not (contains? (car set2) set1)) (union (append set1 (list (car set2))) (cdr set2))]
        [else (union set1 (cdr set2))]))

; Helper method that checks if the element given is present in the list
(define (contains? el li)
    (if (null? li)
      #f
      (if (equal? el (car li))
        #t
        (contains? el (cdr li)))))