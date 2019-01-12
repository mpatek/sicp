(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons
      (/ (if (< (* n d) 0)
	   (- (abs n))
	   (abs n)) g)
      (/ (abs d) g))))
