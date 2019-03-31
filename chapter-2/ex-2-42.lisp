(define empty-board (list))

(define (safe? k positions)
  (define (safe-iter? curr-pos row others)
    (if (null? others) true
      (let ((other-pos (car others)))
	(cond
	  ((= curr-pos other-pos) false)          ; same row
	  ((= curr-pos (+ other-pos row)) false)  ; diagonal down
	  ((= curr-pos (- other-pos row)) false)  ; diagonal up
	  (else (safe-iter? curr-pos (+ row 1) (cdr others)))))))
  (if (null? positions) true
    (if (safe? k (cdr positions))
      (safe-iter? (car positions) 1 (cdr positions))
      false)))

(define (enumerate-interval start end)
  (if (> start end) (list)
    (cons start (enumerate-interval (+ start 1) end))))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
	(lambda (positions) (safe? k positions))
	(flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))
