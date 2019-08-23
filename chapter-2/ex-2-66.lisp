(load "ex-2-63.lisp")

(define (key record) (car record))

(define (lookup given-key tree-of-records)
  (if (null? tree-of-records)
    false
    (let ((entry-key (key (entry tree-of-records))))
      (display entry-key)
      (cond ((= given-key entry-key) (entry tree-of-records))
	    ((< given-key entry-key) (lookup given-key (left-branch tree-of-records)))
	    (else (lookup given-key (right-branch tree-of-records)))))))


(define tree-of-records (make-tree '(5 e)
				   (make-tree '(4 d) () ())
				   (make-tree '(6 f) () ())))
