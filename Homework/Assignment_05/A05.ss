; Natalie Koenig
; CSSE304-03
; Assignment 5
; Prof Buffalo

; #1 
; 
(define (minimize-interval-list ls)
  '())


; #2
; function that returns a list of all elements of lst for which pred? returns true
(define (exists? pred? lst)
   (ormap pred? lst))

; #3
; Function that returns a list of 2 lists
; that represents the Cartesian product of the two
; sets, 2 list order does not matter but list should be in order
(define (product set1 set2)
  (if (null? set1)
    '()
     (if (not (null? set2))
        (append (product-row (car set1) set2) (product (cdr set1) set2))
       '())))

; Function that returns a list of 2 lists for a given element
; where each 2 list contains el along with an element of ls 
(define (product-row el ls)
  (if (null? ls)
    '()
    (append (list (list el (car ls))) (product-row el (cdr ls)))))

; #4 
; Function that replaces the elements in a list with a value 
; equal to old with the value of argument new
(define (replace old new ls)
   (if (null? ls)
      '()
      (if (equal? (car ls) old)
        (append (list new) (replace old new (cdr ls)))
        (append (list (car ls)) (replace old new (cdr ls))))))

; #5 
; Function that removes the last element of the list that is
; equal to the value of element
(define (remove-last element ls)
  (if (null? ls)
    '()
    ))