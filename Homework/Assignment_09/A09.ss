; Natalie Koenig
; CSSE304-03
; Assignment 9
; Prof Buffalo

; Assignment 9

; #1
(define snlist-recur
  (lambda (base-value sym-proc snlist-proc )
    (letrec 
      ([helper 
        (lambda (snlist)
          (cond [(null? snlist) base-value]
                 [(list? (car snlist)) (snlist-proc (helper (car snlist)) (helper (cdr snlist)))]
                 [#t (sym-proc (car snlist) (helper (cdr snlist)))]))])
      helper)))

; #1a
(define (sn-list-sum snlst)
  ((snlist-recur 0 + +) snlst))


; #1b
(define (sn-list-map proc snlst)
  ((snlist-recur '() 
    (lambda (first second)
      (cons (proc first) second)) cons) snlst))

; #1c
(define (sn-list-paren-count snlst)
  (if (list? snlst)
    (+ 2 ((snlist-recur 0 
      (lambda (first second) (+ 0 second))
      (lambda (frst scnd) (+ 2 frst scnd))) snlst))
    0))
      
; #1d
(define (sn-list-reverse snlst)
  (reverse ((snlist-recur '() 
    (lambda (first second) (cons first second))
    (lambda (frst scnd) (cons (reverse frst) scnd))) snlst)))

; #1e
(define (sn-list-occur s snlst)
  ((snlist-recur 0 
    (lambda (first second) 
      (if (equal? first s)
        (+ 1 second)
        (+ 0 second)))
    (lambda (frst scnd)
        (+ frst scnd))) snlst))

; #1f
(define (sn-list-depth snlst)
  (if (list? snlst)
    ((snlist-recur 1
      (lambda (first second) (+ 0 second))
      (lambda (frst scnd) (max (+ 1 frst) scnd))) snlst)
      0))

; #2
(define bt-recur
  (lambda (node-proc tree-proc)
    (letrec 
      ([helper 
        (lambda (bintree)
          (if (null? bintree)
            bintree
            (if (or (number? bintree) (symbol? bintree))
              (node-proc bintree)
              (tree-proc (car bintree) (helper (cadr bintree)) (helper (caddr bintree))))))])
      helper)))

(define (bt-sum bt)
  ((bt-recur + 
    (lambda (node lchild rchild)
      (+ lchild rchild))) bt))

(define (bt-inorder bt)
  ((bt-recur 
    (lambda (node)
        '())
    (lambda (node lchild rchild)
      (append lchild (list node) rchild))) bt))

; (define reverse
;    (lambda (ls)
;      (let rev ((ls ls) (new '())))
;        (if (null? ls)
;           new
;           (rev (cdr ls) (cons (car ls) new)))))

; (define reverse!
;   (lambda (L)
;     (if (null? L)
;       '()
;       (let loop ([L L]
;                  [prev '()]
;                  [next (cdr L)])
;             (set-cdr! L prev)
;             (if (null? next)
;               L
;               (loop next L (cdr next)))))))