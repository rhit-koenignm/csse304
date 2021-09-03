; Natalie Koenig
; CSSE304-03 
; Buffalo

; #1
(define (interval-contains? interval number)
  (and (>= number (car interval)) 
    (<= number (cadr interval))))


; #2
(define (interval-intersects? i1 i2)
  (cond ((>= (car i1) (car i2)) (<= (car i1) (cadr i2)))
        ((< (car i1) (car i2)) (>= (cadr i1) (car i2)))
        (else (#f))))

; #3
(define (interval-union i1 i2)
  (cond ((not (interval-intersects? i1 i2)) (list i1 i2))
        (else (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2))))))