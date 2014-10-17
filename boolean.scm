(define (element? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (#t (element? x (cdr lst)))))

(define (union a b)
  (cond ((null? b) a)
        ((element? (car b) a)
         (union a (cdr b)))
        (#t (union (cons (car b) a) (cdr b)))))


;;;;;;;;;;;;;;

(define (maybe-and a b)
  (cond ((or (equal? '(#f) a) (equal? '(#f) b)) '(#f))
	((or (equal? '() a) (equal? '() b)) '())
	((or (equal? '(#t) a) (equal? '(#t) b)) '(#t))
	(#t (error "bad inputs for maybe-and (~a ~a)" a b))))

(define (maybe-or a b)
  (cond ((or (equal? '(#t) a) (equal? '(#t) b)) '(#t))
	((or (equal? '() a) (equal? '() b)) '())
	((or (equal? '(#f) a) (equal? '(#f) b)) '(#f))))

(define (maybe-and-neg c a-b)
  (cond ((equal? c '(#t)) '(#t))
	((equal? c '()) '())
	((equal? c '(#f))
	 (cond
	   ((equal? a-b '(#t)) '(#f))
	   ((equal? a-b '(#f)) '())
	   ((equal? a-b '()) '())))))

(define (maybe-or-neg  c a-b)
  (cond ((equal? c '(#f)) '(#f))
	((equal? c '()) '())
	((equal? c '(#t))
	 (cond
	   ((equal? a-b '(#f)) '(#t))
	   ((equal? a-b '(#t)) '())
	   ((equal? a-b '()) '())))))


(define (maybe-not a)
  (cond ((equal? a '(#t)) '(#f))
        ((equal? a '(#f)) '(#t))
        ((equal? a '()) '())
        (#t (error "cannot not: " a))))
