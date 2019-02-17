(define (for-each f items)
  (cond ((null? items) items)
	(else
	  (f (car items))
	  (for-each f (cdr items)))))


(for-each (lambda (i) (newline) (display i))
	  (list 1 2 3 5))
