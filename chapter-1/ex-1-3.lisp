(define (ex-1-3 a b c) 
	(if (or (> a b) (> a c))
		(+ (* a a) (if (> b c) (* b b) (* c c)))
		(+ (* b b) (* c c))
	)
)