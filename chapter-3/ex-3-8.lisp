(define (get-f)
  (define f-bal (- (/ 1 2)))
  (define (f incr)
    (set! f-bal (+ f-bal incr))
    f-bal)
  f)

(define f1 (get-f))
(define x (f1 0))
(define y (f1 1))
(+ x y) ; left-to-right = 0

(define f2 (get-f))
(define y (f2 1))
(define x (f2 0))
(+ x y) ; right-to-left = 1
