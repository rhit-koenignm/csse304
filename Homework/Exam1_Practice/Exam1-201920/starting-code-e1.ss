; This code should be part of your solution, since the 
; test-cases depend on it.

(define path-to
  (lambda (slist sym)
    (let pathto ([slist slist] [path-so-far '()])
      (cond [(null? slist) #f]
	    [(eq? (car slist) sym)
	     (reverse (cons 'car path-so-far))]
            [(symbol? (car slist))
	     (pathto (cdr slist) (cons 'cdr path-so-far))]
	    [else
	     (or (pathto (car slist) 
                         (cons 'car path-so-far))
		 (pathto (cdr slist) 
                         (cons 'cdr path-so-far)))]))))

; Put yout Problem 1 solution here
(define symmetric?
  (lambda (matrix)
    (let ([transpose (matrix-transpose matrix)])
      (andmap equal? matrix transpose))))

(define (matrix-transpose m)
    (apply map list m))

; Put yout Problem 2 solution here

(define (sum-of-depths slist)
  (let sum ([slist slist]
            [])))


; Put yout Problem 3 solution here
(define (notate-depth slist) 
  (let notate ([slist slist]
	       [depth 1])
    (cond [(null? slist) '()]
	  [(symbol? (car slist))
	   (cons (list (car slist) depth)
		 (notate (cdr slist) depth))]
	  [else  ; the car is an s-list
	   (cons (notate (car slist) (+ 1 depth))
		 (notate (cdr slist) depth))])))

; (define un-notate
;   (lambda (ls)))


; Put yout Problem 4 solution here




; Put yout Problem 5 solution here




