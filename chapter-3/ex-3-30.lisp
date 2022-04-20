(define (ripple-carry-adder a-wires b-wires sum-wires c-out)
  (if (null? a-wires)
    'ok
    (let ((a (car a-wires))
	  (b (car b-wires))
	  (sum (car sum-wires))
	  (c-in (make-wire)))
      (full-adder a b c-in sum c-out)
      (ripple-carry-adder (cdr a-wires) (cdr b-wires) (cdr sum-wires) c-in))))
