(define random-init 19380110)

(define (rand-update seed)
  (let ((a 69069) (b 1) (m (expt 2 32)))
    (modulo (+ (* seed a) b) m)))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond 
	((eq? m 'generate) (set! x (rand-update x)) x)
	((eq? m 'reset) (lambda (new-value) (set! x new-value)))
	(else (error "Unknown request -- RAND" m))))))


((rand 'reset) 10)
(define a (rand 'generate))
((rand 'reset) 10)
(define b (rand 'generate))

(= a b)
