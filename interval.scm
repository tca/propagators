(require-extension numbers)
(define-record-type <interval>
  (interval min max)
  interval?
  (min interval-min)
  (max interval-max))

(define-record-type <wildcard>
  (wildcard)
  wildcard?)

(define _ (wildcard))

(define (interval->pair i) (cons (interval-min i) (interval-max i)))

(define (num-interval n) (interval n n))

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

(define (interval-conj x y)
  (define (exactness-max x y)
    (if (or (equal? x y) (> x y)) x y))
  (define (exactness-min x y)
    (if (or (equal? x y) (< x y)) x y))
  (interval
   (exactness-max (interval-min x) (interval-min y))
   (exactness-min (interval-max x) (interval-max y))))

(define (interval-disj x y)
  (define (exactness-max x y)
    (if (or (equal? x y) (> x y)) x y))
  (define (exactness-min x y)
    (if (or (equal? x y) (< x y)) x y))
  (interval
   (exactness-min (interval-min x) (interval-min y))
   (exactness-max (interval-max x) (interval-max y))))

(define (interval-sum a b)
  (interval (+-wc (interval-min a) (interval-min b))
	    (+-wc (interval-max a) (interval-max b))))

(define (interval-difference a b)
    (interval (--wc (interval-min a) (interval-min b))
              (--wc (interval-max a) (interval-max b))))

(define (interval-product a b)
  (let ((p1 (*-wc (interval-min a) (interval-min b)))
        (p2 (*-wc (interval-min a) (interval-max b)))
        (p3 (*-wc (interval-max a) (interval-min b)))
        (p4 (*-wc (interval-max a) (interval-max b))))
    (interval (min-wc p1 p2 p3 p4)
	      (max-wc p1 p2 p3 p4))))

(define (interval-quotient x y)
  (if (<= (interval-min y) 0 (interval-max y))
      (error "division by interval with width 0" (interval->pair y))
      (interval-product
       x
       (interval (/-wc 1 (interval-max y))
		 (/-wc 1 (interval-min y))))))
