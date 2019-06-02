(load "ex-2-56.lisp")

(define (augend s)
  (let ((args (cddr s)))
    (if (= (length args) 1)
      (car args)
      (list '+ (car args) (augend (cons '+ args))))))

(define (multiplicand p)
  (let ((args (cddr p)))
    (if (= (length args) 1)
      (car args)
      (list '* (car args) (multiplicand (cons '* args))))))
