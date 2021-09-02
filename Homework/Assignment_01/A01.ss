; Natalie Koenig
; CSSE304-03 
; Buffalo

; #1
(define (interval-contains? i n)
  (and (>= n (car i)) 
    (<= n (cadr i))))


; #2
(define (interval-intersects? i1 i2)
  (cond ((> (car i1) (car i2))(<= (car i1) (cadr i2))
        )(else #f)))

; (define fact
;   (lambda (n)
;     (if (zero? n)
;         1
;         (* n (fact (- n 1))))))

; (define (test-interval-contains?)
;   (let ([correct '(#t #t #f #f #t)]
;         [answers 
;          (list (interval-contains? '(5 8) 6) 
;                (interval-contains? '(5 8) 5)       
;                (interval-contains? '(5 8) 4)
;                (interval-contains? '(5 5) 14)
;                (interval-contains? '(5 5) 5))])
;     (display-results correct answers equal?)))


