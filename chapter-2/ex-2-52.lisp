; wave-w-smile
(segments->painter (list
		    ; upper line
		    (make-segment 0.0 8.0 2.0 6.0)
		    (make-segment 2.0 6.0 3.0 7.0)
		    (make-segment 3.0 7.0 4.0 7.0)
		    (make-segment 4.0 7.0 3.0 8.0)
		    (make-segment 3.0 8.0 4.0 10.0)
		    (make-segment 6.0 10.0 7.0 8.0)
		    (make-segment 7.0 8.0 6.0 7.0)
		    (make-segment 6.0 7.0 7.0 7.0)
		    (make-segment 7.0 7.0 10.0 3.0)
		    ; lower line
		    (make-segment 0.0 7.0 2.0 4.0)
		    (make-segment 2.0 4.0 3.0 6.0)
		    (make-segment 3.0 6.0 4.0 5.0)
		    (make-segment 4.0 5.0 3.0 0.0)
		    (make-segment 4.0 0.0 5.0 2.0)
		    (make-segment 5.0 2.0 6.0 0.0)
		    (make-segment 7.0 0.0 6.0 5.0)
		    (make-segment 6.0 5.0 10.0 2.0)
		    ; smile
		    (make-segment 4.0 8.0 5.0 7.0)
		    (make-segment 5.0 7.0 6.0 8.0)))

; corner-split w/ one copy of up-split and right-split
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1)))
	  (corner (corner-split painter (- n 1))))
      (beside (below painter up)
	      (below right corner)))))

; square-limit w/ faces pointing out
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
				  flip-vert rotate180)))
    (combine4 (corner-split painter n))))
			
