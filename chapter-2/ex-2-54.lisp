(define (equal? x y)
  (cond
    ((and (pair? x) (not (pair? y))) #f)
    ((and (not (pair? x)) (pair? y)) #f)
    ((and (not (pair? x)) (not (pair? y))) (eq? x y))
    (else (and (equal? (car x) (car y))
	       (equal? (cdr x) (cdr y))))))
