(define (identity x) x)

(define (constant x) (lambda ys x))

(define (compose . fns)
  (let comp ((fns fns))
    (cond ((null? fns) 'error)
	  ((null? (cdr fns)) (car fns))
	  (else
	   (lambda args
	     (call-with-values
	       (lambda () (apply (comp (cdr fns)) args))
	       (car fns)))))))

(define (complement f)
  (lambda xs (not (apply f xs))))

(define (swap f)
  (lambda (x y) (f y x)))
