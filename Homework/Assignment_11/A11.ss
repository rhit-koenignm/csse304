; starting code for the last two problems.  
; The file being loaded here should live in the same folder as this one.

(load "chez-init.ss")   

(define-syntax my-let
      (syntax-rules ()
          [(_ ((x v) ...) e1 e2 ...)
              ((lambda (x ...) e1 e2 ...) v ...)]
          [(_ id ((x v) ...) e1 e2 ...)
              ((rec id (lambda (x ...) e1 e2 ...)) v ...)]))

(define-syntax my-and
  (syntax-rules ()
    [(_) #t]
    [(_ exp) exp]
    [(_ e1 e2 ...)
      (if e1
        (my-and e2 ...)
        #f)]))

(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ exp) exp]
    [(_ e1 e2 ...)
      (let ((el e1))
        (if el
          el
          (my-or e2 ...)))]))

(define-syntax +=
  (syntax-rules ()
    [(_ e1 e2)
      (begin (set! e1 (+ e1 e2)) e1)]))

(define-syntax return-first
  (syntax-rules ()
    [(_) #f]
    [(_ exp) exp]
    [(_ e1 e2 ...)
      (let ([n e1])
        n)]))

(define bintree-to-list 
  (lambda (tree)
    (cases bintree tree
      [leaf-node (number) 
      
      (list 'leaf-node number)]
      [interior-node (symbol ltree rtree) (list 'interior-node symbol (bintree-to-list ltree) (bintree-to-list rtree))])))

(define max-interior
  (lambda (tree)
    (let ([maxSym (get-max-and-sym tree)])
      (caddr maxSym))))
  
; Result list is in the format (currentMax, maxMax, maxSymbol)
(define get-max-and-sym
  (lambda (tree)
    (cases bintree tree
      [leaf-node (number) (list number number '())]
      [interior-node (symbol ltree rtree)
        (let* ([leftRes (get-max-and-sym ltree)]
               [rightRes (get-max-and-sym rtree)]
               [currSum (+ (car leftRes) (car rightRes))]
               [maxVal (max (cadr leftRes) (cadr rightRes) currSum)])
          (cond [(and (is-leaf-node? ltree) (is-leaf-node? rtree)) (list currSum currSum symbol)]
                [(equal? (cadr leftRes) maxVal)
                  (if (is-leaf-node? ltree)
                    (list currSum (cadr rightRes) (caddr rightRes))
                    (if (null? (caddr leftRes))
                      (list currSum maxVal symbol)
                      (list currSum maxVal (caddr leftRes))))]
                [(equal? (cadr rightRes) maxVal)
                  (if (is-leaf-node? rtree)
                    (list currSum (cadr leftRes) (caddr leftRes))
                    (if (null? (caddr rightRes))
                      (list currSum maxVal symbol)
                      (list currSum maxVal (caddr rightRes))))]
                [(equal? currSum maxVal) (list currSum maxVal symbol)]
                [else (list currSum maxVal symbol)]))])))


(define is-leaf-node?
  (lambda (tree)
    (cases bintree tree
      [leaf-node (number) #t]
      [interior-node (symbol ltree rtree) #f])))
    

(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))
   
   
(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lambda-exp
   (id symbol?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rand expression?)])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
       [(eqv? (car datum) 'lambda)
	(lambda-exp (car (2nd  datum))
		    (parse-exp (3rd datum)))]
      [else (app-exp (parse-exp (1st datum))
		     (parse-exp (2nd datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

     
; An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda (x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))
(var-exp? (var-exp 'a))
(var-exp? (app-exp (var-exp 'a) (var-exp 'b)))



