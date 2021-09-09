; Natalie Koenig
; CSSE304-03
; Assignment 3
; Prof: Buffalo

; #1
; Recursive function that appends all unique elements from set 2 into set 1
(define (intersection s1 s2)
  (cond [(not (set? s2)) '()]
        [(null? s2) '()]
        [(null? s2) '()]
        [(null? s1) '()]
        [(contains? (car s2) s1) (append (list (car s2)) (intersection s1 (cdr s2)))]
        [else (intersection s1 (cdr s2))]))

; #2
; Recursive function that checks if the first list
; is a subset of the second
(define (subset? s1 s2)
  (cond [(null? s1) #t]
        [(null? s2) #f]
        [(contains? (car s1) s2) (and #t (subset? (cdr s1) s2))]
        [else #f]))

; #3
; Function that determines if the list of ordered 
; pairs is a relation
(define (relation? rel)
  (cond [(or (not (list? rel)) (not (set? rel))) #f]
        [(null? rel) #t]
        [(and (list? (car rel)) (equal? (length (car rel)) 2)) (relation? (cdr rel))]
        [else #f]))

; Helper function to check if an ordered pair is contained in a relation
(define (contains-pair? rel li)
  (let ([r (car li)])
    (cond [(null? li) #f]
        [(and (equal? (car rel) (car r)) (equal? (cadr rel) (cadr r))) #t]
        [else (contains-relation? rel (cadr li))])))


; #4 
; function that finds the domain of the given relation
(define (domain r)
  (if (or (not (relation? r)) (null? r))
    '()
    (get-x-vals r '())))

; Helper function that gets all the x values
(define (get-x-vals rel vals)
  (if (null? rel)
    vals
    (let ([added (append vals (list (car (car rel))))])
      (if (not (contains? (car (car rel)) vals))
        (get-x-vals (cdr rel) added)
        (get-x-vals (cdr rel) vals)))))
  
; #5
; 
(define (reflexive? rel)
  (if (null? rel)
    #t
    (reflexive-help rel rel)))

(define (reflexive-help li rel)
  (if (null? li)
    #t
    (if (subset? (list (list (car (car li)) (car (car li)))) rel)
      (if (subset? (list (list (cadr (car li)) (cadr (car li)))) rel)
        (reflexive-help (cdr li) rel)
        #f)
        #f)))

; #6 

(define (multi-set? li)
  (if (null? li)
    #t
    (if (relation? li)
      (if (and (list? (car li)) (not (integer? (car (car li)))) (equal? (length li) (length (get-x-vals li '()))) (integer? (cadr (car li))) (positive? (cadr (car li))))
        (multi-set? (cdr li))
      #f)
      #f)))


; #7
; function finding the size of the multi-set using apply and map
(define (ms-size ms)
  (apply + (map cadr ms)))

; #8
; recursive function that returns the last element in the list
(define (last li)
  (cond [(null? li) '()]
         [(equal? (length li) 1) (car li)]
         [else (last (cdr li))]))

; #9
; recursive function that returns all but the last element in the list
(define (all-but-last li)
  (cond [(null? li) '()]
        [(equal? (length li) 1) (all-but-last (cdr li))]
        [else (append (list (car li)) (all-but-last (cdr li)))]))

; Previous assignment's code


; Helper method that checks if the element given is present in the list
(define (contains? el li)
    (if (null? li)
      #f
      (if (equal? el (car li))
        #t
        (contains? el (cdr li)))))

; recursively goes through the list and determines if there are repeated
; elements, meaning it would not be a set
(define (set? li)
    (if (or (not (list? li)) (null? li))
      #t
      (if (contains? (car li) (cdr li))
        #f
        (set? (cdr li)))))

(define (first li)
  (car li))

(define (second li)
  (cadr li))

(define (third li)
  (caddr li)) 

; (define ins-sort
;   (lambda (lon)
;     (if (or (null? lon) (null? (cdr lon)))
;       lon
;       (insert (car lon) (ins-sort (cdr lon)))))

; (define insert
;   (lambda (n sorted-list)
;     (cond 
;       [(null? sorted-list) (list n)]
;       )))