(define (fast-expt b n)
	(cond ((= n 0) 1)
		(else (fast-expt-iter b n 1 b))))

(define (fast-expt-iter b n nc bc)
	(cond ((= n nc) bc)
		((<= (* 2 nc) n) (fast-expt-iter b n (* 2 nc) (square bc)))
		(else (fast-expt-iter b n (+ 1 nc) (* b bc)))))
