(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?)
	(error "FRONT called with an empty queue" front-ptr)
	(car front-ptr)))

    (define (insert! item)
      (let ((new-pair (cons item '())))
	(cond ((empty?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair)
	       front-ptr)
	      (else
		(set-cdr! rear-ptr new-pair)
		(set! rear-ptr new-pair)
		front-ptr))))

    (define (delete!)
      (cond ((empty?)
	     (error "DELETE! called with an empty queue" front-ptr))
	    (else
	      (set! front-ptr (cdr front-ptr))
	      front-ptr)))
    (define (dispatch m)
      (cond ((eq? 'empty? m) empty?)
	    ((eq? 'front m) front)
	    ((eq? 'insert! m) insert!)
	    ((eq? 'delete! m) delete!)
	    (else
	      (error "DISPATCH called with unknown operand" m))))
    dispatch))
