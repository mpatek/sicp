(define (square-tree-direct tree)
  (if (null? tree)
    tree
    (if (pair? tree)
      (cons (square-tree-direct (car tree))
	    (square-tree-direct (cdr tree)))
      (square tree))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (square-tree-map sub-tree)
	   (square sub-tree)))
       tree))
