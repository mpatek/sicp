(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))

(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (make-deque-node item) (list item '() '()))

(define (set-next-node! node1 node2) (set-car! (cdr node1) node2))

(define (set-prev-node! node1 node2) (set-car! (cdr (cdr node1)) node2))

(define (next-node node) (car (cdr node)))

(define (prev-node node) (car (cdr (cdr node))))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called with an empty deque" deque)
    (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called with an empty deque" deque)
    (car (rear-ptr deque))))

(define (insert-rear-deque! deque item)
  (let ((new-node (make-deque-node item)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-node)
	   (set-rear-ptr! deque new-node)
	   )
	  (else
	    (set-next-node! (rear-ptr deque) new-node)
	    (set-prev-node! new-node (rear-ptr deque))
	    (set-rear-ptr! deque new-node)
	    ))))

(define (insert-front-deque! deque item)
  (let ((new-node (make-deque-node item)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-node)
	   (set-rear-ptr! deque new-node)
	   )
	  (else
	    (set-prev-node! (front-ptr deque) new-node)
	    (set-next-node! new-node (front-ptr deque))
	    (set-front-ptr! deque new-node)
	    ))))

(define (delete-front-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE-FRONT! called with an empty deque" deque))
	(else
	  (set-front-ptr! deque (next-node (front-ptr deque)))
	  )))

(define (delete-rear-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE-REAR! called with an empty deque" deque))
	(else
	  (set-rear-ptr! deque (prev-node (rear-ptr deque)))
	  )))
