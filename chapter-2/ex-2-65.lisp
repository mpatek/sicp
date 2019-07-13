(load "ex-2-63.lisp")
(load "ex-2-64.lisp")

(define (union-list-set set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond
	  ((= x1 x2) (union-list-set (cdr set1) set2))
	  ((< x1 x2) (cons x1 (union-list-set (cdr set1) set2)))
	  (else (cons x2 (union-list-set set1 (cdr set2)))))))))

(define (intersection-list-set set1 set2)
  (if (or (null? set1) (null? set2))
    ()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond
	((= x1 x2) (cons x1 (intersection-list-set (cdr set1) (cdr set2))))
	((< x1 x2) (intersection-list-set (cdr set1) set2))
	(else (intersection-list-set set1 (cdr set2)))))))


(define (union-set set1 set2)
  (list->tree (union-list-set
		(tree->list-1 set1)
		(tree->list-1 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-list-set
		(tree->list-1 set1)
		(tree->list-1 set2))))
