(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (if (null? set2)
    set1
    (union (adjoin-set (car set2) set1)
	   (cdr set2))))

(define (intersection-set set1 set2)
  (cond ((null? set1) set1)
	((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
