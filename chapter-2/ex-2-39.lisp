(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
	    (cdr rest))))
  (iter initial sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (fold-right cons (list x) y)) (list) sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))
