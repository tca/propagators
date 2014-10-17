;; requires scheme-kore

(define p (new-prompt))

(define (box x) (cons x '()))
(define (unbox b) (car b))
(define (set-box! b v) (set-car! b v))

(define-record-type <cell>
  (make-cell value observers merge)
  cell?
  (value cell-value)
  (observers cell-observers)
  (merge cell-merge))

(define (merge! cell-box value)
  (let ((cell (unbox cell-box)))
    (push-prompt p
      (let ((new-value ((cell-merge cell) (cell-value cell) value)))
        (let ((new-cell (make-cell new-value (cell-observers cell) (cell-merge cell))))
          (set-box! cell-box new-cell)
          (for-each run-agent! (cell-observers cell)))))))


(define (bi-rel a->b b->a)
  (lambda (a b)
    (let ((au (lambda () (merge! a (b->a (ref-cell b)))))
	  (bu (lambda () (merge! b (a->b (ref-cell a))))))
      (add-observer! a bu)
      (add-observer! b au))))

;; if we move prompt outside merge we can even avoid merging empty values
(define (tri-rel combine remove)
  (lambda (a b c)
    (let ((au (lambda () (merge! a (push-prompt p (list (remove (ref-cell c) (ref-cell b)))))))
	  (bu (lambda () (merge! b (push-prompt p (list (remove (ref-cell c) (ref-cell a)))))))
	  (cu (lambda () (merge! c (push-prompt p (list (combine (ref-cell a) (ref-cell b))))))))
      (add-observer! a bu)
      (add-observer! a cu)
      (add-observer! b au)
      (add-observer! b cu)
      (add-observer! c au)
      (add-observer! c bu))))

(define (add-observer! cell-box obs)
  (let ((cell (unbox cell-box)))
    (let ((new-observers (cons obs (cell-observers cell))))
      (set-box! cell-box (make-cell (cell-value cell) new-observers (cell-merge cell))))))

(define (run-agent! a) (a))

(define (ref-cell cell-box)
  (let ((v (cell-value (unbox cell-box))))
    (if (null? v)
        (shift p k '())
        (car v))))

(define (eq-merge a b)
  (cond ((and (null? a) (null? b)) (shift p k '()))
        ((null? a) b)
        ((null? b) a)
        ((equal? (car a) (car b)) (shift p k '()))
        (else (error "CANNOT MERGE" a b))))


;;;;;;;;;;;;;;;;
;; arithmetic ;;
;;;;;;;;;;;;;;;;


(define a (box (make-cell '() '() eq-merge)))
(define b (box (make-cell '() '() eq-merge)))
(define c (box (make-cell '() '() eq-merge)))
((tri-rel + -) a b c)


(merge! a '(1))
(merge! b '(1))
(ref-cell c)
;; => 2


;;;;;;;;;;;;;
;; boolean ;;
;;;;;;;;;;;;;
(load "./boolean.scm")

(define (merge-maybool a b)
  (let ((new (cond ((and (null? a) (null? b)) (shift p k '()))
                   ((null? a) (car b))
                   ((null? b) (car a))
                   (else (union (car a) (car b))))))
    (cond ((<= (length new) 1) (if (equal? a (list new))
                                   (shift p k '())
                                   (list new)))
	  (#t (error "cannot merge ~a <- ~a" a b)))))

;;;;;;;

(define not-agent (bi-rel maybe-not maybe-not))
(define and-agent (tri-rel maybe-and maybe-and-neg))
(define or-agent (tri-rel maybe-or maybe-or-neg))


(define d (box (make-cell '() '() merge-maybool)))
(define e (box (make-cell '() '() merge-maybool)))
(define f (box (make-cell '() '() merge-maybool)))

(and-agent d e f)
(merge! d '((#t)))
(merge! e '((#f)))
(ref-cell f)
;; => (#f)

;;;;;;;;;;;;;;;
;; INTERVALS ;;
;;;;;;;;;;;;;;;

(load "./interval.scm")

;; (some x) + (none) = (some x)
;; need to know closed/open because '() + 1 might be error
(define (maybify-merge op)
  (lambda (a b)
    (let ((new (cond
                ((and (null? a) (null? b)) (shift p k '()))
                ((null? a) (list (car b)))
                ((null? b) (list (car a)))
                (else (list (op (car a) (car b)))))))
      (if (equal? a new)
          (shift p k '())
          new))))

(define merge-interval (maybify-merge interval-conj))
(define merge-disj-interval (maybify-merge interval-disj1))

;;;;;;

(define interval-==-agent (bi-rel identity identity))
(define interval-sum-agent (tri-rel interval-sum interval-difference))
(define interval-product-agent (tri-rel interval-product interval-quotient))
(define interval-conj-agent (tri-rel interval-conj interval-conj))
(define interval-disj-agent (tri-rel interval-disj interval-conj))

(define g (box (make-cell '() '() merge-interval)))
(define h (box (make-cell '() '() merge-interval)))
(define i (box (make-cell '() '() merge-interval)))

(interval-sum-agent g h i)
(merge! g `(,(interval 1 2)))
(merge! h `(,(interval 1 2)))
(display (interval->pair (ref-cell i)))
