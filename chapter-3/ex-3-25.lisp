(define (assoc key records)
  (cond ((null? records) false)
	((eq? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup keys)
      (let ((record (assoc (car keys) (cdr local-table))))
	(if record
	  (let ((value (caddr record)))
	    (cond ((null? (cdr keys)) value)
		  ((is-table-record? record) ((value 'lookup) (cdr keys)))
		  (else false)))
	  false)))

    (define (get-new-record keys value)
      (if (null? (cdr keys))
	(list 'not-a-table value)
	(let ((subtable (make-table)))
	  ((subtable 'insert!) (cdr keys) value)
	  (list 'table subtable))))

    (define (is-table-record? record)
      (eq? (cadr record) 'table))

    (define (insert! keys value)
      (let ((record (assoc (car keys) (cdr local-table)))
	    (new-record (get-new-record keys value)))
	(if record
	  (set-cdr! record new-record)
	  (set-cdr! local-table (cons (cons (car keys) new-record) (cdr local-table))))))

    (define (dispatch msg)
      (cond ((eq? msg 'lookup) lookup)
	    ((eq? msg 'insert!) insert!)
	    (else (error "Unknown operation -- TABLE" msg))))
    dispatch))

(define t (make-table))
((t 'insert!) '(a b c) 1)
((t 'lookup) '(a b c))
((t 'insert!) '(a b) 1)
((t 'lookup) '(a b))
