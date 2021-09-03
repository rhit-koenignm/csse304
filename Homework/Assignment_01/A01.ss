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
        (else (list (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2)))))))


; #4
(define (first li)
  (car li))

(define (second li)
  (cadr li))

(define (third li)
  (caddr li))    

; #5
(define (make-vec-from-points p1 p2)
  (list (- (first p2) (first p1)) (- (second p2) (second p1)) (- (third p2) (third p1))))

; #6
(define (dot-product p1 p2)
  (+ (* (first p2) (first p1)) (* (second p2) (second p1)) (* (third p2) (third p1))))

; #7 
(define (vector-magnitude v)
  (sqrt (+ (expt (first v) 2)
        (expt (second v) 2)
        (expt (third v) 2))))

; #8
(define (distance p1 p2)
  (sqrt (+ (expt (- (first p2) (first p1)) 2)
        (expt (- (second p2) (second p1)) 2)
        (expt (- (third p2) (third p1)) 2))))