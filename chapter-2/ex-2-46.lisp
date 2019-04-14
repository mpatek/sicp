(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect u v)
  (make-vect 
    (+ (xcor-vect u) (xcor-vect v))
    (+ (ycor-vect u) (ycor-vect v))))

(define (scale-vect f v)
  (make-vect
    (* f (xcor-vect v))
    (* f (ycor-vect v))))

(define (sub-vect u v)
  (add-vect u (scale-vect -1 v)))
