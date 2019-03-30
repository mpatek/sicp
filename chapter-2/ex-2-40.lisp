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

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
	   (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (divides? a b )
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (define (next test-divisor)
    (cond ((= test-divisor 2) 3)
	  (else (+ test-divisor 2))))
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))
