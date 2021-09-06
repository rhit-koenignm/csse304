; Natalie Koenig
; CSSE304-03
; Assignment 3
; Prof: Buffalo

; #1

; #5
; Recursive function that appends all unique elements from set 2 into set 1
(define (intersection s1 s2)
  (cond [(null? s2) '()]
        [(null? s1) '()]
        [(contains? (car s2) s1) (intersection (append s1 (list (car s2))) (cdr s2))]
        [else (intersection s1 (cdr s2))]))


; Helper methods and previous assignment's code

; Helper method that checks if the element given is present in the list
(define (contains? el li)
    (if (null? li)
      #f
      (if (equal? el (car li))
        #t
        (contains? el (cdr li)))))
