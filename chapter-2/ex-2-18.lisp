(define (reverse items)
  (define (reverse-iter list1 list2)
    (if (null? list1)
      list2
      (reverse-iter (cdr list1) (cons (car list1) list2))))
  (reverse-iter items '()))
