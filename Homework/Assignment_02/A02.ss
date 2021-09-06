; Natalie Koenig
; CSSE304-03
; Assignment 2
; Prof: Buffalo

; #1

; a)
; recursive factorial procedure
; parameter n: must be a non-zero integer
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

; b)
; nonrecursive procedure that utilizes the
; recursive function "fact"
; parameter n: must be a non-zero integer
; parameter k: must be a non-zero integer
(define choose 
  (lambda (n k)
    (/ (fact n) (* (fact k) (fact (- n k))))))

; #2 
; recursive sum of squares procedure
; parameter lon: must be a single-level list of numbers
(define sum-of-squares 
  (lambda (lon) 
    (cond [(null? lon) 0]
          [#t (+ (* (car lon) (car lon)) (sum-of-squares (cdr lon)))])))

; #3 
; recursive ordered list of ints
; parameter m: must be an integer
; paramater n: must be an integer
(define range 
  (lambda (m n)
    (if (<= n m)
      '()
      (cons m (range (+ m 1) n)))))

; #4
; recursively goes through the list and determines if there are repeated
; elements, meaning it would not be a set
(define (set? li)
    (if (null? li) 
      #t
      (if (contains? (car li) (cdr li))
        #f
        (set? (cdr li)))))
    

; Helper method that checks if the element given is present in the list
(define (contains? el li)
    (if (null? li)
      #f
      (if (equal? el (car li))
        #t
        (contains? el (cdr li)))))

; #5
; Recursive function that appends all unique elements from set 2 into set 1
(define (union set1 set2)
  (cond [(null? set2) set1]
        [(null? set1) set2]
        [(not (contains? (car set2) set1)) (append set1 (list (car set2)) (union set1 (cdr set2)))]
        [else (append set1 (union set1 (cdr set2)))]))

; #6
; Function that calculates the cross product using column vectors such as
; s1 = a2*b3 - a3*b2
; s2 = a3*b1 - a1*b3
; s3 = a1*b2 - a2*b1
(define (cross-product v1 v2)
  (list [- (* (second v1) (third v2)) (* (third v1) (second v2))]
        [- (* (third v1) (first v2)) (* (first v1) (third v2))]
        [- (* (first v1) (second v2)) (* (second v1) (first v2))]))

; #7
; Function that determines if two vectors are parallel by looking for
; a common scalar multiple
(define (parallel? v1 v2)
  (let ([cross (cross-product v1 v2)])
    (cond [(and (equal? 0 (first cross)) (equal? 0 (second cross))) (equal? 0 (third cross))]
          [else #f])))

; (define (parallel? v1 v2)
;   (let ([s1 (/ (first v2) (first v1))]
;         [s2 (/ (second v2) (second v1))]
;         [s3 (/ (third v2) (third v1))])
;     (cond [(equal? s1 s2) (equal? s2 s3)]
;       [else #f])))

; #8
; Function that determines if three points are collinear
(define (collinear? p1 p2 p3)
  (let ([s1 (/ (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))]
        [s2 (/ (- (cadr p3) (cadr p1)) (- (car p3) (car p1)))]
        [s3 (/ (- (cadr p3) (cadr p2)) (- (car p3) (car p2)))])
      (cond [(equal? s1 s2) (equal? s1 s3)]
            [else #f])))

; Helper functions from A01
(define (first li)
  (car li))

(define (second li)
  (cadr li))

(define (third li)
  (caddr li))    

(define (make-vec-from-points p1 p2)
  (list (- (first p2) (first p1)) (- (second p2) (second p1)) (- (third p2) (third p1))))

(define (dot-product p1 p2)
  (+ (* (first p2) (first p1)) (* (second p2) (second p1)) (* (third p2) (third p1))))

(define (vector-magnitude v)
  (sqrt (+ (expt (first v) 2)
        (expt (second v) 2)
        (expt (third v) 2))))

(define (distance p1 p2)
  (sqrt (+ (expt (- (first p2) (first p1)) 2)
        (expt (- (second p2) (second p1)) 2)
        (expt (- (third p2) (third p1)) 2))))

