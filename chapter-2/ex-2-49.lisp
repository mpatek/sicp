; frame outline
(segments->painter (list
		     (make-segment 0 0 0 1)
		     (make-segment 0 1 1 1)
		     (make-segment 1 1 1 0)
		     (make-segment 1 0 0 0)))

; X
(segments->painter (list
		    (make-segment 0 0 1 1)
		    (make-segment 0 1 1 0)))

; diamond
(segments->painter (list
		    (make-segment 0 0.5 0.5 1)
		    (make-segment 0.5 1 1 0.5)
		    (make-segment 1 0.5 0.5 0)
		    (make-segment 0.5 0 0 0.5)))

; wave
(segments->painter (list
		    ; upper line
		    (make-segment 0 8 2 6)
		    (make-segment 2 6 3 7)
		    (make-segment 3 7 4 7)
		    (make-segment 4 7 3 8)
		    (make-segment 3 8 4 10)
		    (make-segment 6 10 7 8)
		    (make-segment 7 8 6 7)
		    (make-segment 6 7 7 7)
		    (make-segment 7 7 10 3)
		    ; lower line
		    (make-segment 0 7 2 4)
		    (make-segment 2 4 3 6)
		    (make-segment 3 6 4 5)
		    (make-segment 4 5 3 0)
		    (make-segment 4 0 5 2)
		    (make-segment 5 2 6 0)
		    (make-segment 7 0 6 5)
		    (make-segment 6 5 10 2)))
