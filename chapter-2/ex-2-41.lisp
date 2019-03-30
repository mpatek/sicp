(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval start end)
  (if (> start end)
    (list)
    (cons start (enumerate-interval (+ start 1) end))))

(define (unique-triples n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j) (
			map (lambda (k) (list i j k))
			(enumerate-interval 1 (- j 1))))
	   (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (triple-sum triple)
  (+ (car triple) (cadr triple) (caddr triple)))

(define (triples-of-sum n s)
  (filter (lambda (triple) (= (triple-sum triple) s))
	  (unique-triples n)))
