(define (get-f)
  (define f
    (let ((counter 0))
      (lambda (x)
	(let ((answer (/ (abs (- x counter)) 2)))
	  (set! counter (+ counter 1))
	  answer))))
  f)

(define f1 (get-f))
(+ (f1 0) (f1 1))  ; = 0

(define f2 (get-f))
(+ (f2 1) (f2 0))  ; = 1
