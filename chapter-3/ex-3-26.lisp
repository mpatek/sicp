(define (make-table)

  (let ((tbl (list "*table*")))
    (define (get-tree)
      (cdr tbl))
    (define (is-empty?)
      (null? (get-tree)))
    (define (get-key)
      (caar (get-tree)))
    (define (get-value)
      (cdar (get-tree)))
    (define (get-left-branch)
      (car (cdr (get-tree))))
    (define (get-right-branch)
      (car (cdr (cdr (get-tree)))))
    (define (set-tree! tree)
      (set-cdr! tbl tree))
    (define (set-value! value)
      (set-cdr! (car (get-tree)) value))

    (define (lookup key)
      (cond
	((is-empty?) false)
	((= (get-key) key) (get-value))
	((< (get-key) key) (((get-left-branch) 'lookup) key))
	(else (((get-right-branch) 'lookup) key))))

    (define (insert! key value)
      (cond
	((is-empty?) (set-tree! (list (cons key value) (make-table) (make-table))))
	((= (get-key) key) (set-value! value))
	((< (get-key) key) (((get-left-branch) 'insert!) key value))
	(else (((get-right-branch) 'insert!) key value))))

    (define (dispatch msg)
      (cond
	((eq? msg 'lookup) lookup)
	((eq? msg 'insert!) insert!)
	(else (error "Unknown operation -- TABLE" msg))))

    dispatch))

(define a (make-table))
((a 'lookup) 1)
((a 'insert!) 1 2)
((a 'lookup) 1)
((a 'insert!) 1 3)
((a 'lookup) 1)
((a 'lookup) 2)
((a 'insert!) 2 4)
((a 'lookup) 2)
((a 'lookup) 0)
((a 'insert!) 0 100)
((a 'lookup) 0)
