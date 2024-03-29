(define (make-account balance secret-passwd)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (withdraw amount)
    (cond ((>= balance amount)
	   (set! balance (- balance amount))
	   balance)
	  (else "Insufficient funds")))
  (define call-the-cops (lambda () "Calling the cops!"))
  (let ((ntries 0))
    (define (dispatch passwd m)
      (cond 
	((eq? passwd secret-passwd)
	 (set! ntries 0)
	 (cond ((eq? m 'deposit) deposit)
	       ((eq? m 'withdraw) withdraw)
	       (else (error "Unknown request -- MAKE-ACCOUNT"
			    m))))
	(else
	  (set! ntries (+ ntries 1))
	  (cond ((<= ntries 7) (error "Incorrect password -- MAKE-ACCOUNT"))
		(else (call-the-cops))))))
    dispatch))
