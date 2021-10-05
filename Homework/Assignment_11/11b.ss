(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
    (id symbol?)]
  
  [lambda-exp
    (id symbol?)
    (body expression?)]

  [app-exp
    (rator expression?)
    (rand (list-of expression?))]
    
  [lit-exp
    (id (lambda (x) #t))]

  [if-exp
    (condition expression?)
    (truebody expression?)
    (falsebody expression?)]

  [let-exp
    (vardec (list-of expression?))
    (values (list-of expression?))]

  [let*-exp
    (vardec (list-of expression?))
    (values (list-of expression?))]
  
  [let-rec-exp
    (vardec (list-of expression?))
    (values (list-of expression?))]

  [set!-exp
    (id symbol?)
    (value expression?)]
  )

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
(define after2nd cddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(pair? datum)
        (cond
          [(eqv? (1st datum) 'lambda)
            (cond
              [(or (< (length datum) 3) (> (length datum) 3)) (eopl:error 'parse-exp "lambda expression: incorrect length: ~s" datum)]
              [(list? (2nd datum)) 
                (if (not (andmap symbol? (2nd datum))) 
                  (eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" (2nd datum))
                  (lambda-exp (car (2nd  datum)) (parse-exp (3rd datum))))]                    
                  [(= (length datum) 3) (lambda-exp (car (2nd  datum)) (parse-exp (3rd datum)))]
              [else (lambda-exp (2nd datum) (parse-exp (after2nd datum)))]
              )]

          [(eqv? (1st datum) 'let)
            (cond 
              [(< (length datum) 3) (eopl:error 'parse-exp "Error in parse-expression: let expression: incorrect length: ~s" datum)]
              [(list? (2nd datum))
                (cond 
                  [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: not all proper lists: " (2nd datum))]                 
                  [(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "Error in parse-exp: let expression: first members must be symbols: ~s" datum)] 
                  [(not (andmap (lambda (x) (equal? (length x) 2)) (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: let expression: not all length 2: ~s" datum)]
                  [else (let-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-expression: letrec expression: declarations is not a list")]
              [else (let-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]
          
          [(eqv? (1st datum) 'let*)
            (cond 
              [(< (length datum) 3) (eopl:error 'parse-exp "Error in parse-expression: let expression: incorrect length: ~s" datum)]
              [(list? (2nd datum))
                (cond 
                  [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: not all proper lists: " (2nd datum))]                 
                  [(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "Error in parse-exp: let expression: first members must be symbols: ~s" datum)] 
                  [(not (andmap (lambda (x) (equal? (length x) 2)) (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: let expression: not all length 2: ~s" datum)]
                  [else (let*-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-expression: letrec expression: declarations is not a list")]
              [else (let*-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]
          
          [(eqv? (1st datum) 'letrec)
            (cond 
              [(< (length datum) 3) (eopl:error 'parse-exp "Error in parse-expression: let expression: incorrect length: ~s" datum)]
              [(list? (2nd datum))
                (cond 
                  [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: not all proper lists: " (2nd datum))]                 
                  [(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "Error in parse-exp: let expression: first members must be symbols: ~s" datum)] 
                  [(not (andmap (lambda (x) (equal? (length x) 2)) (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: let expression: not all length 2: ~s" datum)]
                  [else (let-rec-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]
              [(not (list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-expression: letrec expression: declarations is not a list")]
              [else (let-rec-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]

          [(eqv? (1st datum) 'set!)
            (cond [(> (length datum) 3) (eopl:error 'parse-exp "Too many parts: ~s" datum)]
                  [(< (length datum) 3) (eopl:error 'parse-exp "Missing expression: ~s" datum)]
                  [else (set!-exp (2nd datum) (parse-exp (3rd datum)))])]
                  
          [(eqv? (1st datum) 'if)
            (cond [(< (length datum) 4) (eopl:error 'parse-exp "if expression should have (only) test, then, and else clauses:" datum)]
                  [else (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))])]
          
          [else (if (list? datum)
                (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))
                (eopl:error 'parse-exp "not a proper list: ~s" datum))])]

     [(null? datum) '()]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
    (lambda (x)
      (cases expression x
        [var-exp (id) id]
        [lit-exp (id) id]
        [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
        [set!-exp (id value) (list 'set! id (unparse-exp value))]
        [let-exp (vardec values) (list 'let (map unparse-exp vardec) (map unparse-exp values))]
        [let*-exp (vardec values) (list 'let* (map unparse-exp vardec) (map unparse-exp values))]
        [let-rec-exp (vardec values) (list 'letrec (map unparse-exp vardec) (map unparse-exp values))]
        [lambda-exp (id body) (append (list 'lambda (list id)) (unparse-exp body))]
        [else x])))

; An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda (x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))
     
; (var-exp? (var-exp 'a))
; (var-exp? (app-exp (var-exp 'a) (var-exp 'b)))
