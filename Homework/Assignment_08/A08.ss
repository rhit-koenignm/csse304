; Natalie Koenig
; CSSE304-03
; Assignment 8
; Prof Buffalo

; Assignment 8

; #1
; Function that iterates through slist leafs

(define (make-slist-leaf-iterator slist)
  (let ([iter-stack (make-stack)])
    (begin (iter-stack 'push slist)
      (lambda (msg) 
        (case msg ; Scheme's case is a similar to switch in some other languages.
            [(next) (get-next iter-stack)]
            [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

(define (get-next stk)
  (if (stk 'empty?)
    #f
    (let ([current (stk 'pop)])
      (cond [(null? current) (get-next stk)]
            [(symbol? (car current)) (begin (stk 'push (cdr current)) 
                                            (car current))]
            [else (begin (stk 'push (cdr current))
                         (stk 'push (car current))
                         (get-next stk))]))))

(define make-stack
(lambda ()
 (let ([stk '()])
 (lambda (msg . args ) 
   (case msg ; Scheme's case is a similar to switch in some other languages.
    [(empty?) (null? stk)]
    [(push) (set! stk (cons (car args) stk))]
    [(pop) (let ([top (car stk)])
 (set! stk (cdr stk))
 top)]
 [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

; #2
; Function 
(define (subst-leftmost new old slist predicate)
  (if (null? slist)
    slist
    (let ([first (car slist)]
        [rest (cdr slist)])
      (cond [(null? first) (subst-leftmost new old rest predicate)]
            [(symbol? first) 
                (if (predicate old first) (cons new rest)
                  (cons first (subst-leftmost new old rest predicate)))]
            [(list? first)
              (let ([result (did-replace? new old first predicate)])
                (if (list? (car (car result)))
                    (if (list? (car (car (car result))))
                        (if (equal? (cadr (car (car result))) #t)
                            (cons (list (list (car (car result)))) rest)
                            (cons first (subst-leftmost new old rest predicate)))
                        (if (equal? (cadr (car result)) #t)
                            (cons (list (car (car result))) rest)
                            (cons first (subst-leftmost new old rest predicate))))
                    (if (equal? (cadr result) #t)
                          (cons (car result) rest)
                          (cons first (subst-leftmost new old rest predicate)))))]
            [else slist]))))

(define (did-replace? new old slist predicate)
  (if (null? slist)
    (list slist #f)
    (let ([first (car slist)]
          [rest (cdr slist)])
      (cond [(null? first) (did-replace? new old rest predicate)]
            [(symbol? first) 
              (if (predicate old first) 
                  (list (cons new rest) #t)
                  (list (cons first (subst-leftmost new old rest predicate)) #f))]
            [(list? first) (list (did-replace? new old (car first) predicate))]
            [else (did-replace? new old rest predicate)]))))