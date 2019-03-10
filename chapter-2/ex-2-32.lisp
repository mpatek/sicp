(define (subsets s)
  (if (null? s)
    (list (list))
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; For each element of the set
; we include all the subsets without that element
; along with all the subsets with that element
; and that should mean we include all subsets.
