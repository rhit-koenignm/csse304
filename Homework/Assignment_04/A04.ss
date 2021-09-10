; Natalie Koenig
; CSSE304-03
; Assignment 4
; Prof: Buffalo


; #1 
; A function that fetches the value at the position row col
(define (matrix-ref m  row col)
    (let ([newRowM (cdr m)]
          [newColM (cons (cdr (car m)) (cdr m))])
      (if (null? m)
        '()
        (if (zero? row )
          (if (zero? col)
            (car (car m))
            (matrix-ref newColM row (- col 1)))
          (matrix-ref newRowM (- row 1 ) col)))))

; #2 
; A function that checks if the passed in object is a matrix
; A matrix is defined as a nonempty list of nonempty lists of numbers
(define (matrix? obj)
   (if (and (list? obj) (list? (car obj)))
     (if (and (>= (length obj) 1) (>= (length (car obj))))
       (if (has-valid-rows? obj (length (car obj)))
          #t
          #f)
        #f)
    #f))

; Function that uses recursion to determine if all the 
; rows have the same length and are all numbers
(define (has-valid-rows? m len)
    (if (null? m)
      #t
      (if (and (not (null? (car m))) (andmap number? (car m)))
        (if (equal? (length (car m)) len)
            (has-valid-rows? (cdr m) len)
           #f)
        #f)))

; #3 
; Function that transposes the given matrix, 
; assume argument is matrix
(define (matrix-transpose m)
    (apply map list m))


; (define (matrix-transpose m)
;   (if (or (null? m) (null? (car m)))
;     '()
;     (append (list (reverse (car m))) (matrix-transpose (cdr m)))))

(define (get-transpose-col li)
   (if (null? li)
      (append '())
      (append (car li) (get-transpose-col (cdr li))))
)

(define (get-transpose-row li)
    (if (null? li)
       (append '())
       (append (list (list (car li))) (get-transpose-row (cdr li)))))

; #4
; function that returns a list of all elements of lst for which pred? returns true
(define (filter-in pred? lst)
   (cond [(null? lst) '()]
         [(pred? (car lst)) (append (list (car lst)) (filter-in pred? (cdr lst)))]
         [else (filter-in pred? (cdr lst))]))

; #5 
; function that reverses each row in a matrix 
(define (invert li)
   (cond [(null? li) '()]
         [else (append (list (reverse (car li))) (invert (cdr li)))]))


; #6
; function that creates a pascal's triangle
(define (pascal-triangle n)
  (if (< n 0)
    '()
    (if (zero? n)
        (list (pascal-row 0 0))
        (append (list (pascal-row 0 n)) (pascal-triangle (- n 1))))))

; function that creates a list representing a row 
; in our triangle, where p is the starting position (0)
(define (pascal-row p r)
  (cond [(equal? p r) (list (pascal-number r p))]
        [else (append (list (pascal-number r p)) (pascal-row (+ p 1) r))]))

; function that finds the value at the given row 
; and column of our triangle
(define (pascal-number r c)
   (cond [(zero? c) 1]
         [(zero? r) c]
         [else (/ (* r (pascal-number (- r 1) (- c 1))) c)]))

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