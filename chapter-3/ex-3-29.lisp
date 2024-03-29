(define (or-gate a1 a2 output)
  (let ((n1 (make-wire))
	(n2 (make-wire))
	(n3 (make-wire)))
    (inverter a1 n1)
    (inverter a2 n2)
    (and-gate n1 n2 n3)
    (inverter n3 output)
    'ok))

; delay will be inverter-delay + and-gate-delay + inverter-delay
