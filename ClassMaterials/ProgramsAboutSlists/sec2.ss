;Examples going on in Session 7

; (define (contains? slist sym)
;   (let in-list? ([slist slist]) ; We'll use named-let a lot today
;     (cond [(null? slist) #f]
;           [(symbol? (car slist))   
;              (if (eq? (car slist) sym)
;                 #t
;                 (in-list? (cdr slist)))]
;           [else (or (in-list (car slist)) (in-list? (cdr slist)))])))


; (define (count-occurences slist sym) 
;     (cond [(null? slist) 0]
;           [(symbol? (car slist))
;               (+ (if (eq? (car slist) sym) 1 0) 
;                 (count-occurences cdr slist sym))]
;           [else (+ (count-occurences (car slist) sym)
;                    (count-occurences (cdr slist) sym))]))


; (define (flatten slist)
;   (cond [(null? slist) '()]
;         [(symbol? (car slist))
;           (cons (car slist) (flatten (cdr slist)))]
;         [else 
;           (append (flatten (car slist)) (flatten (cdr slist))])))

; (define (notate-depth slist)
;   (let notate ([slist slist]
;         [depth 1])
;       (cond [(null? slist) '()]
;             [(symbol? (car slist)) (cons (list (car slist) depth)
;                                     (notate (cdr slist) depth))]
;             [else (cons (notate (car slist) (add1 depth)) (notate (cdr slist) depth))])))


;             (define make-array-list
;               (lambda L 
;                 (let ([v (make-vector 3 #f)]
;                       [capacity 3]
;                       [size 0])
;                     (cond [null? L) 'do-nothing]
;                           [(integer? (car L))
;                             (set! v (car L))
;                             (set! capacity (vector-length (car L)))
;                             (set! size capacity))
;                             ]
;                           [(vector? (car L))
;                             (set! v (car L))
;                             (set! capacity (vector-length (car L)))
;                             (set! size capacity)]
;                           [else (errorf 'array-list-constructor "initial arguments")])
;                           (letrec 
;                           (
;                               [ensure-capacity 
;                                 (lambda (new-cap) 
;                                     (if 
;                                       (< capacity new-cap)
;                                       (let ((oldv v))
;                                         (set! v (make-vector (* 2 new-cap) #f))
;                                         (set! capacity (* 2 new-cap))
;                                         (copy-elements oldv v size))))]
;                               [copy-elements (lambda (old new size))]

;                           )
;                           (lambda (method . args)
;                             (case method 
;                               [(add insert-at-end)
;                                (ensure-capacity (+ 1 size))
;                                (if (null? (cdr args)) 
;                                (vector-set! v size (car args))
;                                'You-will-add-the-other-case)))
;                                 (set! size (+ size 1))]
;                             ))


(define list-length 
  (letrec 
    ([helper 
        (lambda (ls)
          (if (null? ls)
            0
            (+ 1 (helper (cdr ls)))))])
    helper))