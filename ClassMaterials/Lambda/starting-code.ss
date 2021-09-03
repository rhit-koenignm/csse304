; Day 2 in-class exercises

; (median-of-three a b c) returns the median of the three numbers.
(define median-of-three
  (lambda (a b c)
    (- (+ a b c) (max a b c) (min a b c))))

; Tests
(median-of-three 3 2 6) ; 3
(median-of-three 1 2 3) ; 2
(median-of-three 3 2 1) ; 2
(median-of-three 1 2 1) ; 1

; (add-n-to-each n lon) Given a list of numbers lon and a number n,
;   return a new list of numbers where each element is n more than
;   the corresponding element of lon.

(define add-n-to-each 
  (lambda (n lon)
    (if 
        (null? lon)
        '()
        (cons (+ n (car lon)) (add-n-to-each n (cdr lon))))))

; Tests
(add-n-to-each 5 '())         ; ()
(add-n-to-each 4 '(3 5 7 9))  ; (7 9 11 13)

; (count-occurrences n lon) counts how many times n appears in lon

(define count-occurrences
  (lambda (n lon)
    (cond 
        [(null? lon) 0]
        [(equal? (car lon) n) (+ 1 (count-occurrences n (cdr lon)))]
        [#t (count-occurrences n (cdr lon))])))

; Tests
(count-occurrences 3 '())          ; 0
(count-occurrences 2 '(2 3 2 4 2)) ; 3
(count-occurrences 2 '(3 3 2 4 2)) ; 2
(count-occurrences 2 '(2 3 2 4 3)) ; 2
(count-occurrences 7 '(1 2 3 4 5)) ; 0

; (square-sum n) returns sum of the squares of the first n positive integers

(define square-sum 
  (lambda (n)
    (cond [(<= n 0) 0]
          [#t (+ (* n n) (square-sum (- n 1)))])))

; Tests
(square-sum 1)  ; 1
(square-sum 2)  ; 5
(square-sum 3)  ; 14
(square-sum 4)  ; 30
(square-sum 50) ; 42925

; (square-all lon) returns a list of the squares of the numbers in lon



; Tests
(square-all '())         ; ()
(square-all '(3 -2 0 4)) ; (9 4 0 16)

; (make-list n obj) returns a list of n "copies" of obj.
;  [If obj is a "by-reference" object, such as a list,
;  make-list makes n copies of the reference].



; Tests
(make-list 3 '(a b)) ; ((a b) (a b) (a b))
(make-list 0 9)      ; ()


