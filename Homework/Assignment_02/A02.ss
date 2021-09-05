; Natalie Koenig
; CSSE304-03 
; Buffalo


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
; recursively 
(define (set? li)
    (if (null? li) 
      #t
      (if (contains? (car li) (cdr li))
        #f
        (set? (cdr li)))))
    

; Helper method for detecting if it is a set
(define (contains? el li)
    (if (null? li)
      #f
      (if (equal? el (car li))
        #t
        (contains? el (cdr li)))))