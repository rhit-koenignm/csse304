; Natalie Koenig
; CSSE304-03
; Assignment 7a
; Prof Buffalo

; Note: Worked with Melina Ferner on the last 3

; Assignment 7

; #1
; Function that uses helper functions copy-from-vector and
; copy-from-list to create a vector with vec's and ls's content
(define (vector-append-list vec ls)
   (let ([newV (make-vector (+ (vector-length vec) (length ls)))])
     (copy-from-vector newV vec 0 (vector-length vec))
     (copy-from-list newV ls (vector-length vec))
     newV))

; Function that moves the old vector's content into newV
(define (copy-from-vector newV old pos length) 
  (if (< pos length)
    (let ([vecEl (vector-ref old pos)])
        (vector-set! newV pos vecEl)
        (copy-from-vector newV old (+ pos 1) length))))

; Function that moves ls's content into the new vector
(define (copy-from-list newV ls pos)
  (if (not (null? ls))
    (let ([newPos (+ pos 1)]
          [lsEl (car ls)]
          [nextList (cdr ls)])
      (vector-set! newV pos lsEl)
      (copy-from-list newV nextList newPos))))


; #2 
; Function to convert list into a list of lists of size 2
(define (group-by-two ls)
  (if (null? ls)
    '()
    (let ([newGroup (group-helper ls '() 0 '() 2)])
      newGroup)))
    
; Recursive helper function that splits up a list into
; a list of lists of grpSize
(define (group-helper ls newLs index sub grpSize)
  (if (null? ls)
    (if (not (null? sub))
      (append newLs (list sub))
       newLs)
    (let ([element (car ls)]
          [nextLs (cdr ls)]
          [nextIndex (+ index 1)])
        (if (equal? (- grpSize index) 1)
          (let ([subLs (append newLs (list (append sub (list element))))])
            (group-helper nextLs subLs 0 '() grpSize))
          (group-helper nextLs newLs nextIndex (append sub (list element)) grpSize)))))

; #3 
; Function that splits a list into a list of
; lists with length n
(define (group-by-n ls n)
  (if (null? ls)
    '()
    (let ([newGroup (group-helper ls '() 0 '() n)])
      newGroup)))

; #4
; F
(define (get-root-node ls)
  (if (null? ls)
    '()
    (car ls)))

(define (get-left-subTree ls)
  (if (null? ls)
    '()
    (cadr ls)))

(define (get-right-subTree ls)
  (if (null? ls)
    '()
    (caddr ls)))

(define (is-BST? T)
  (if (list? T)
    (if (equal? (length T) 3)
      #t
      #f)
    #f))

; Helper function that examines whether T is a symbol with two integers
; for subtrees
(define (is-leaf-node? T)
  (if (and (number? (get-left-subTree T)) (number? (get-right-subTree T)))
    #t
    #f))

(define (bt-leaf-sum T) 
  (if (list? T)
    (+ (bt-leaf-sum (cadr T)) (bt-leaf-sum (caddr T)))
    T))
    
(define (bt-inorder-list ls)
  (if (and (list? ls) (not (number? (car ls))))
    (if (is-leaf-node? ls)
      (list (car ls))
      (append (bt-inorder-list (get-left-subTree ls)) (list (car ls)) (bt-inorder-list (get-right-subTree ls))))
    '()))

; I can't believe this works so well tbh
(define (bt-max T)
  (if (list? T)
    (max (bt-max (cadr T)) (bt-max (caddr T)))
    T))

; The next couple functions help me solve bt-max-interio

(define (bt-max-interior T)
 (let ([maxSym (get-max-and-sym T)])
       (caddr maxSym)
    ))

; Result list is in the format (currentMax, maxMax, maxSymbol)
(define get-max-and-sym
 (lambda (T)
  (if (number? T)
    (list T T '())
    (let* ([leftRes (get-max-and-sym (cadr T))]
           [rightRes (get-max-and-sym (caddr T))]
           [currSum (+ (car leftRes) (car rightRes))]
           [maxVal (max (cadr leftRes) (cadr rightRes) currSum)])
      (cond [(is-leaf-node? T) (list currSum currSum (car T))]
            [(eq? (cadr leftRes) maxVal)
              (if (number? (cadr T))
                (list currSum (cadr rightRes) (caddr rightRes))
                (if (null? (caddr leftRes))
                    (list currSum maxVal (car T))
                    (list currSum maxVal (caddr leftRes))))]             
            [(eq? (cadr rightRes) maxVal)
                (if (number? (caddr T))
                  (list currSum (cadr leftRes) (caddr leftRes))
                   (if (null? (caddr rightRes))
                    (list currSum maxVal (car T))
                    (list currSum maxVal (caddr rightRes))))]             
            [(eq? currSum maxVal) (list currSum maxVal (car T))]
            [else (list currSum maxVal (car T))])))))
    
; #5a 
; Recursive function that maps a procedure to all elements of slist
(define (slist-map proc slist) 
  (if (null? slist)
    '()
    (if (list? slist)
      (if (list? (car slist))
        (append (list (slist-map proc (car slist))) (slist-map proc (cdr slist)))
        (append (slist-map proc (car slist)) (slist-map proc (cdr slist))))
      (append (list (proc slist))))))

; #5b
;Recursive function that reverses the contents of slist and any sublists within
(define (slist-reverse slist)
  (cond [(null? slist) slist]
        [(list? (car slist)) (append (slist-reverse (cdr slist)) (list (slist-reverse (car slist))))]
        [else (append (slist-reverse (cdr slist)) (list (car slist)))]))

; #5c
; Recursive function that finds the number of parentheses required to
; produce printed rep of slist
(define (slist-paren-count slist)
  (cond [(null? slist) 2]
        [(list? (car slist)) (+ (slist-paren-count (cdr slist)) (slist-paren-count (car slist)))]
        [else (+ 0 (slist-paren-count (cdr slist)))]))


; #5d
(define (slist-depth slist)
  (define (depth-helper depth slist)
    (define rest (filter list? slist))
    (if (zero? (length rest))
        depth
        (depth-helper (add1 depth) (apply append rest))))
  (depth-helper 1 slist))

; #5e
(define (slist-symbols-at-depth slist d)
    (cond [(null? slist) '()]
          [(symbol? (car slist)) 
            (if (equal? d 1)
              (cons (car slist) (slist-symbols-at-depth (cdr slist) d))
              (slist-symbols-at-depth (cdr slist) d))]
          [else (append (slist-symbols-at-depth (car slist) (sub1 d)) 
                        (slist-symbols-at-depth (cdr slist) d))]))

; #6
(define (path-to slist sym)
  (cond [(null? slist) #f]
        [(symbol? (car slist))
          (if (equal? (car slist) sym)
            (list 'car)
            (let ((result (path-to (cdr slist) sym)))
              (if result
                (cons 'cdr result)
                #f)))]
        [#t 
          (let ((result2 (path-to (car slist) sym)))
            (if result2
              (cons 'car result2)
              (let ((result3 (path-to (cdr slist) sym)))
                (if result3
                  (cons 'cdr result3)
                  #f))))]))

; #7
(define (make-c...r str)
  (let* ([args (string->list str)]
         [lst (map (lambda (char) (string #\c char #\r)) args)])
         (apply compose (map (lambda (boo) (eval (string->symbol boo))) lst))))

(define compose
 (case-lambda
 [() (lambda (x) x)]
 [(first . rest)
 (let ([composed-rest (apply compose rest)])
 (lambda (x) (first (composed-rest x))))]))

; (define (path-to slist sym)
;   (cond [(null? slist) #f]
;         [(list? (car slist)) (append (list 'cdr) (path-to (car slist) sym) (path-to (cdr slist) sym))]
;         [(and (symbol? (car slist)) (equal? (car slist) sym)) (append (list 'car))]
;         [(symbol? (car slist)) (append (list 'cdr) (path-to (cdr slist) sym))]
;         [else '()]))

; (define (path slist sym)
;   (cond [(null? slist) #f]
;         [(list? ())]))


