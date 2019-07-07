(load "ex-2-56.lisp")

(define (in-expr expr sym)
  (and (pair? expr)
       (not (null? expr))
       (or (eq? (car expr) sym)
	   (in-expr (cdr expr) sym))))

(define (sum? x) (in-expr x '+))

(define (product? x)
  (and (in-expr x '*) (not (sum? x))))

(define (exponentiation? x)
  (and (in-expr x '**) (not (or (sum? x) (product? x)))))

(define (maybe-scalar expr)
  (if 
    (and (pair? expr) (= (length expr) 1)) (car expr)
    expr))

(define (after-sym expr sym)
  (if (eq? (car expr) sym)
    (maybe-scalar (cdr expr))
    (after-sym (cdr expr) sym)))

(define (maybe-reverse expr)
  (if (pair? expr) (reverse expr) expr))

(define (before-sym expr sym)
  (define (before-sym-helper expr acc)
    (if (eq? (car expr) sym)
      (maybe-reverse (maybe-scalar acc))
      (before-sym-helper (cdr expr) (cons (car expr) acc))))
  (before-sym-helper expr ()))

(define (addend s) (before-sym s '+))
(define (augend s) (after-sym s '+))
(define (multiplier p) (before-sym p '*))
(define (multiplicand p) (after-sym p '*))
(define (base s) (before-sym s '**))
(define (exponent s) (after-sym s '**))
