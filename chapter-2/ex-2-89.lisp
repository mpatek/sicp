(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (if (= (order term) (length term-list))
      (cons (coef term) term-list)
      (adjoin-term term (cons 0 term-list)))))

(define (first-term term-list) (make-term (length (cdr term-list)) (car term-list)))

; rest-terms ensures that we end up with a list where first term is non-zero
(define (rest-terms term-list)
  (let ((r (cdr term-list)))
    (cond
      ((null? r) r)
      ((not (=zero? (car r))) r)
      (else (rest-terms r)))))

(define (empty-termlist? term-list) (null? term-list))
