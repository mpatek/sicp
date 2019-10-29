(load "ex-2-67.lisp")

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    (list)
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
		  (make-leaf-set (cdr pairs))))))

(define (successive-merge trees)
  (if (null? (cdr trees))
    (car trees)
    (successive-merge
      (adjoin-set (make-code-tree (car trees) (cadr trees))
		  (cddr trees)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree '((a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)))
