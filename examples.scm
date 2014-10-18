(load "./propagator.scm")

(define a (int-cell))
(define b (int-cell))
(define c (int-cell))
(sum-agent a b c)

(merge! a '(1))
(merge! b '(1))
(ref-cell c)
;; => 2


(define d (bool-cell))
(define e (bool-cell))
(define f (bool-cell))

(and-agent d e f)
(merge! d '((#t)))
(merge! e '((#f)))
(ref-cell f)
;; => (#f)

(define g (interval-cell))
(define h (interval-cell))
(define i (interval-cell))

(interval-sum-agent g h i)
(merge! g `(,(interval 1 2)))
(merge! h `(,(interval 1 2)))
(display (interval->pair (ref-cell i)))
