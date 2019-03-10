(define (make-mobile left right)
  (list left right))

(define (make-structure length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-branch-weight branch)
  (if (not (pair? (branch-structure branch)))
    (branch-structure branch)
    (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (total-branch-weight (left-branch mobile))
     (total-branch-weight (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch) (total-branch-weight branch)))

(define (balanced? mobile)
  (and
    (= (torque (left-branch mobile)) (torque (right-branch mobile)))
    (or (not (pair? (branch-structure (left-branch mobile))))
	(balanced? (branch-structure (left-branch mobile))))
    (or (not (pair? (branch-structure (right-branch mobile))))
	(balanced? (branch-structure (right-branch mobile))))))
