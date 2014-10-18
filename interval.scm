(define-record-type <interval>
  (interval min max)
  interval?
  (min interval-min)
  (max interval-max))

(define-record-type <wildcard>
  (wildcard)
  wildcard?)

(define _ (wildcard))

(define (interval->pair i)
  (cons (interval-min i) (interval-max i)))

(define (num-interval n)
  (interval n n))

(define (<=-wc a b)
  (or (wildcard? a) (wildcard? b) (<= a b)))

(define (valid-intervalp min max)
  (or (wildcard? min)
      (wildcard? max)
      (>= max min)))

(define (wc-empty/2 op)
  (lambda (a b)
    (cond ((wildcard? a) b)
          ((wildcard? b) a)
          (else (op a b)))))

(define (wc-inf/2 op)
  (lambda (a b)
    (cond ((wildcard? a) a)
          ((wildcard? b) b)
          (else (op a b)))))

(define (reducer op) (lambda (f . r) (foldl op f r)))

(define min-wc (reducer (wc-empty/2 min)))
(define max-wc (reducer (wc-empty/2 max)))
(define +-wc (reducer (wc-inf/2 +)))
(define --wc (reducer (wc-inf/2 -)))
(define *-wc (reducer (wc-inf/2 *)))
(define /-wc (reducer (wc-inf/2 /)))

(define (interval-conj a b)
  (interval (min-wc (interval-min a) (interval-min b))
	    (max-wc (interval-max a) (interval-max b))))

(define (interval-disj a b)
  (interval (max-wc (interval-min a) (interval-min b))
	    (min-wc (interval-max a) (interval-max b))))

(define (interval-disj1 a b)
  (let ((nmin (max-wc (interval-min a) (interval-min b)))
	(nmax (min-wc (interval-max a) (interval-max b))))
    (interval nmin (max nmax nmin))))

(define (interval-sum a b)
  (interval (+-wc (interval-min a) (interval-min b))
	    (+-wc (interval-max a) (interval-max b))))

(define (interval-difference a b)
  (let ((min (--wc (interval-min a) (interval-min b)))
	(max (--wc (interval-max a) (interval-max b))))
    (interval (max-wc min (interval-max a)) (max-wc min max))))

(define (interval-product a b)
  (let ((p1 (*-wc (interval-min a) (interval-min b)))
        (p2 (*-wc (interval-min a) (interval-max b)))
        (p3 (*-wc (interval-max a) (interval-min b)))
        (p4 (*-wc (interval-max a) (interval-max b))))
    (interval (min-wc p1 p2 p3 p4)
	      (max-wc p1 p2 p3 p4))))

(define (interval-quotient x y)
  (if (= (/ (- (interval-max y) (interval-min y)) 2) 0)
      (error "division by interval with width 0") ;; or nothing
      (interval-product
       x
       (interval (/ 1 (interval-max y))
		 (/ 1 (interval-min y))))))
