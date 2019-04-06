(define (split op-1 op-2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split op-1 op-2) painter (- n 1))))
	(op-1 painter (op-2 smaller smaller))))))

(define up-split (split below beside))
(define right-split (split beside below))
