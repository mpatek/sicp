(load "ex-2-67.lisp")

(define (encode-symbol s tree)
  (define (encode-symbol-1 s tree bits)
    (if (leaf? tree)
      (if (eq? (symbol-leaf tree) s) bits #f)
      (let ((left-bits (encode-symbol-1 s (left-branch tree) bits)))
	(if (not (eq? left-bits #f))
	  (cons 0 left-bits)
	  (let ((right-bits (encode-symbol-1 s (right-branch tree) bits)))
	    (if (not (eq? right-bits #f))
	      (cons 1 right-bits)
	      #f))))))
  (let ((bits (encode-symbol-1 s tree (list))))
    (if (not (eq? bits #f))
      bits
      (error "bad symbol -- ENCODE-SYMBOL" s))))

(define (encode message tree)
  (if (null? message)
    (list)
    (append (encode-symbol (car message) tree)
	    (encode (cdr message) tree))))

(encode (decode sample-message sample-tree) sample-tree)
