;; raise base b to the integer power e; e must be non-negative
(define (ipow b e)
  (if (= e 0) 1
      (let loop ((s b) (i e) (a 1)) ; a * s^i = b^e
	(let ((a (if (odd? i) (* a s) a)) (i (quotient i 2)))
	  (if (zero? i) a (loop (* s s) i a))))))

;; The integer square root of a positive number is the greatest integer that,
;; when multiplied by itself, does not exceed the given number. The integer
;; square root can be calculated by Newton's method of approximation via 
;; derivatives.
(define (isqrt n)
  (if (not (and (positive? n) (integer? n)))
      (error 'isqrt "argument must be a positive integer")
      (let loop ((x n))
	(let ((y (quotient (+ x (quotient n x)) 2)))
	  (if (< y x) (loop y) x)))))

;; The integer logarithm base b of a number n is the number of times the
;; number b can be multiplied by itself without exceeding n
(define (ilog b n)
  (let loop1 ((lo 0) (b^lo 1) (hi 1) (b^hi b))
    (if (< b^hi n)
	(loop1 hi b^hi (* hi 2) (* b^hi b^hi))
	(let loop2 ((lo lo) (b^lo b^lo) (hi hi) (b^hi b^hi))
	  (if (<= (- hi lo) 1)
	      (if (= b^hi n) hi lo)
	      (let* ((mid (quotient (+ lo hi) 2))
		     (b^mid (* b^lo (expt b (- mid lo)))))
		(cond ((< n b^mid) (loop2 lo b^lo mid b^mid))
		      ((< b^mid n) (loop2 mid b^mid hi b^hi))
		      (else mid))))))))

;; Modular exponentiation is equivalent to (modulo (expm b e) m) except that
;; the algorithm avoids the calculation of the large intermediate exponentiation
;; by performing multiply-and-square stages.
(define (expm b e m)
  (define (m* x y) (modulo (* x y) m))
  (cond ((zero? e) 1)
	((even? e) (expm (m* b b) (/ e 2) m))
	(else (m* b (expm (m* b b) (/ (- e 1) 2) m)))))

(define (digits n . args)
  (let ((b (if (null? args) 10 (car args))))
    (let loop ((n n) (d '()))
      (if (zero? n)
	  d
	  (loop (quotient n b) (cons (modulo n b) d))))))

(define (undigits ds . args)
  (let ((b (if (null? args) 10 (car args))))
    (let loop ((ds ds) (n 0))
      (if (null? ds)
	  n
	  (loop (cdr ds) (+ (* n b) (car ds)))))))
