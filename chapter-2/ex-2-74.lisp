;; assumes that empfile has been tagged with a 
;; with division-specific tag using attach-tag
;; and each division has registered their own get-record operator
;; like this:
;; (put 'get-record div-empfile-tag div-get-record)
(define (get-record empfile empid)
  (let ((op (get 'get-record (type-tag empfile))))
    (op empfile empid)))

;; assumes that emprecord has been tagged with
;; division-specific tag using attach-tag
;; and each division has registered their own get-salary operator
;; like this:
;; (put 'get-salary '(div-emprecord) div-get-salary)
(define (get-salary emprecord)
  (apply-generic 'get-salary emprecord))

(define (find-employee-record empfiles empid)
  (if (null? empfiles)
    (list)
    (let ((emp-record (get-record (car empfiles) empid)))
      (if (null? emp-record)
	(find-employee-record (cdr empfiles) empid)
	emp-record))))
