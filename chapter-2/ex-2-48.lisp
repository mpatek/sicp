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

(define (make-segment start-x start-y end-x end-y)
  (cons (make-vect start-x start-y)
	(make-vect end-x end-y)))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))
