(define (f-recursive n)
	(if (< n 3)
		n
		(+
			(f-recursive (- n 1))
			(* 2 (f-recursive (- n 2)))
			(* 3 (f-recursive (- n 3))))))

(define (f-iter n)
	(define (f-iter-helper a b c counter)
		(if (= 0 counter) a
			(f-iter-helper b c (+ c (* 2 b) (* 3 a)) (- counter 1))))
	(f-iter-helper 0 1 2 n))