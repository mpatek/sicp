(define (pascal row col)
	(if (or (= 1 col) (= row col)) 1
		(+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))
