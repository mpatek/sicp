(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (fringe items)
  (cond ((null? items) items)
	(else
	  (if (pair? (car items))
	    (append (fringe (car items)) (fringe (cdr items)))
	    (cons (car items) (fringe (cdr items)))))))
