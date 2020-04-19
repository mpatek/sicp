(define (install-term-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (tag x) (attach-tag 'term x))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  (put 'make-term 'term (lambda (order coeff) (tag (make-term order coeff)))))

(define (install-sparse-term-list-packages)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist) '())

  (define (tag x) (attach-tag 'sparse-term-list x))
  (put 'adjoin-term '(term sparse-term-list) adjoin-term)
  (put 'first-term '(sparse-term-list) first-term)
  (put 'rest-terms '(sparse-term-list) rest-terms)
  (put 'empty-termlist? '(sparse-term-list) empty-termlist?)
  (put 'make-sparse-termlist 'sparse-term-list (lambda () (tag (the-empty-termlist)))))

(define (install-dense-term-list-package)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (if (= (order term) (length term-list))
	(cons (coeff term) term-list)
	(adjoin-term term (cons 0 term-list)))))
  (define (first-term term-list) (make-term (length term-list) (car term-list)))
  (define (rest-terms term-list)
    (let ((r (cdr term-list)))
      (cond
	((null? r) r)
	((not (=zero? (car r))) r)
	(else (rest-terms (cdr r))))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist) '())

  (define (tag x) (attach-tag 'dense-term-list x))
  (put 'adjoin-term '(term dense-term-list) adjoin-term)
  (put 'first-term '(dense-term-list) first-term)
  (put 'rest-terms '(dense-term-list) rest-terms)
  (put 'empty-termlist? '(dense-term-list) empty-termlist?)
  (put 'make-dense-termlist 'dense-term-list (lambda () (tag (the-empty-termlist)))))

(define (make-sparse-termlist) (get 'make-sparse-termlist 'sparse-term-list))
(define (make-dense-termlist) (get 'make-dense-termlist 'dense-term-list))

(define (the-empty-termlist) (make-sparse-termlist))  ; default to sparse representation

(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
