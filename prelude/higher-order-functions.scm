(define (identity x)
  "The identify function: always returns its argument."
  x)

(define (constant x)
  "Returns a function that always returns the given value."
  (lambda ys x))

(define (compose . fns)
  "Returns a function that applies the given list of functions in order to the list of arguments given to it."
  (let comp ((fns fns))
    (cond ((null? fns) 'error)
	  ((null? (cdr fns)) (car fns))
	  (else
	   (lambda args
	     (call-with-values
	       (lambda () (apply (comp (cdr fns)) args))
	       (car fns)))))))

(define (complement f)
  "Returns a function that inverts the true/false value of the given function."
  (lambda xs (not (apply f xs))))

(define (swap f)
  "Returns a function that applies the given function but swaps the order of the arguments."
  (lambda (x y) (f y x)))

(define (partial f arg)
  "Returns a function that is the partial application of the given function to the given argument."
  (lambda args (apply f arg args)))
