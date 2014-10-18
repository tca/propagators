(load "~/projects/scheme-kore/delimcc.scm")
(load "~/projects/scheme-kore/parameters.scm")
(load "~/projects/scheme-kore/pmatch.scm")

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

;;;;;;;;;;;;;;;;
;; arithmetic ;;
;;;;;;;;;;;;;;;;

(define sum-agent (tri-rel + -))
(define (int-cell) (box (make-cell '() '() eq-merge)))

;;;;;;;;;;;;;
;; boolean ;;
;;;;;;;;;;;;;
(load "./boolean.scm")

(define merge-maybool
  (maybify-merge
   (lambda (a b)
     (let ((new (union a b)))
       (if (<= (length new) 1)
           new
           (#t (error "cannot merge ~a <- ~a" a b)))))))

;;;;;;;

(define not-agent (bi-rel maybe-not maybe-not))
(define and-agent (tri-rel maybe-and maybe-and-neg))
(define or-agent (tri-rel maybe-or maybe-or-neg))

(define (bool-cell) (box (make-cell '() '() merge-maybool)))

;;;;;;;;;;;;;;;
;; INTERVALS ;;
;;;;;;;;;;;;;;;

(load "./interval.scm")


(define merge-interval (maybify-merge interval-conj))
(define merge-disj-interval (maybify-merge interval-disj1))

;;;;;;

(define interval-==-agent (bi-rel identity identity))
(define interval-sum-agent (tri-rel interval-sum interval-difference))
(define interval-product-agent (tri-rel interval-product interval-quotient))
(define interval-conj-agent (tri-rel interval-conj interval-conj))
(define interval-disj-agent (tri-rel interval-disj interval-conj))

(define (interval-cell) (box (make-cell '() '() merge-interval)))
(define (interval-cell-growing) (box (make-cell '() '() merge-disj-interval)))
