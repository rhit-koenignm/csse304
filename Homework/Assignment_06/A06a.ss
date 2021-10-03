; Natalie Koenig
; CSSE304-03
; Assignment 6b
; Prof Buffalo

; #1 
; 
(define curry2 
  (lambda (x) 
    (lambda (a)
      (lambda (b)
        (x a b)))))

; #2
(define curried-compose
  (lambda (x)
    (lambda (y) 
      (lambda (a)
        (x (y a))))))

; #3
(define compose
  (lambda list-of-functions
    (lambda (x)  
        (if (null? list-of-functions)
          x
          (if (equal? (length list-of-functions) 1)
            ((car list-of-functions) x)
            (if (equal? (length list-of-functions) 2)
              ((car list-of-functions) ((cadr list-of-functions) x))
              (if (equal? (length list-of-functions) 3) 
                ((car list-of-functions) ((cadr list-of-functions) ((caddr list-of-functions) x)))
                ((car list-of-functions) ((apply compose (cdr list-of-functions)) x)))))))))

; #4
; Makes a list with the value of x repeated n times
(define make-list-c 
  (lambda (n)
    (lambda (x)
        (if (<= n 0)
            '()
            (append (list x) ((make-list-c (- n 1)) x))))))

; #5 
; 
(define reverse-it 
  (lambda (lst)
    (cond [(null? lst) '()]
          [else (append (reverse-it (cdr lst)) (list (car lst)))])))

; #6
;
(define map-by-position 
  (lambda (fn-list arg-list)
       (map (lambda (func arg)
          (func arg)) fn-list arg-list)))

; #7
; These are my BST functions used to represent a Binary Search Tree
(define (empty-BST) 
  (list))

(define (empty-BST? obj)
  (null? obj))

(define (BST-insert num bst)
  (cond [(empty-BST? bst) (list num '() '())]
        [(equal? num (car bst)) bst]
        [(> num (car bst)) (list (car bst) (BST-left bst) (BST-insert num (BST-right bst)))]
        [else (list (car bst) (BST-insert num (BST-left bst)) (BST-right bst))]))

(define (BST-inorder bst)
  (if (not (empty-BST? bst))
    (append (BST-inorder (BST-left bst)) (list (car bst)) (BST-inorder (BST-right bst)))
    (append '())))

(define (BST? obj)
  (cond [(empty-BST? obj) #t]
        [(not (list? obj)) #f]
        [(not (equal? (length obj) 3)) #f]
        [(not (number? (car obj))) #f]
        [(and (list? (BST-left obj)) (not (empty-BST? (BST-left obj))) (>= (car (BST-inorder (BST-left obj))) (car obj))) #f]
        [(and (list? (BST-right obj)) (not (empty-BST? (BST-right obj))) (<= (car (BST-inorder (BST-right obj))) (car obj))) #f]
        [else (and (BST? (BST-left obj)) (BST? (BST-right obj)))]))

(define (BST-element bst)
  (if (empty-BST? bst)
    '()
    (car bst)))

(define (BST-left bst)
  (if (empty-BST? bst)
    '()
    (cadr bst)))

(define (BST-right bst)
  (if (empty-BST? bst)
    '()
    (caddr bst)))

(define (BST-insert-nodes bst nums)
  (if (null? nums)
    bst
    (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums))))

(define (BST-contains? bst num)
  (if (empty-BST? bst)
    #f
    (if (equal? (car bst) num)
      #t
      (if (>= num (car bst))
        (BST-contains? (BST-right bst) num)
        (BST-contains? (BST-left bst) num)))))

(define (BST-height bst)
  (if (empty-BST? bst)
    -1
    (+ 1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst))))))


; #8
; 
(define (let->application lst)
  (append (list (list 'lambda (map car (cadr lst)) (caddr lst))) (map cadr (cadr lst))))  

; #9

; (define (let*->let lst)
;   (list 'let (list (car (cadr lst))) (list 'let (list (cdadr lst)) (caddr lst))))

(define (let*->let lst)
  (cond [(equal? (length (cadr lst)) 1) (append (list 'let) (cdr lst))]
        [else (append (list 'let) (list (list (caadr lst))) (list (let*->let (append (list 'let) (list (cdadr lst)) (cddr lst)))))]))

; #10
(define (qsort pred ls)
  (if (or (null? ls) (null? (cdr ls)))
    ls
    (let* ([pv (car ls)]
          [remaining (cdr ls)]
          [lsIn (filter-in pred pv remaining)]
          [lsOut (filter-out pred pv remaining)])
        (append (qsort pred lsIn) (list pv) (qsort pred lsOut)))))

; helper function 
; function that returns a list of all elements of lst for which pred? returns true
(define (filter-in pred? pv lst)
   (cond [(null? lst) '()]
         [(pred? (car lst) pv) (append (list (car lst)) (filter-in pred? pv (cdr lst)))]
         [else (filter-in pred? pv (cdr lst))]))

; function that returns a list of all elements of lst for which pred? returns true
(define (filter-out pred? pv lst)
   (cond [(null? lst) '()]
         [(not (pred? (car lst) pv)) (append (list (car lst)) (filter-out pred? pv (cdr lst)))]
         [else (filter-out pred? pv (cdr lst))]))

; #11
(define (sort-list-of-symbols los)
  (map string->symbol (list-sort string<? (map symbol->string los))))