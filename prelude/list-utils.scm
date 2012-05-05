(define (take n xs)
  (let loop ((n n) (xs xs) (ys '()))
    (if (or (zero? n) (null? xs))
	(reverse ys)
	(loop (- n 1) (cdr xs) (cons (car xs) ys)))))

(define (drop n xs)
  (let loop ((n n) (xs xs))
    (if (or (zero? n) (null? xs))
	xs
	(loop (- n 1) (cdr xs)))))

(define (split n xs)
  (let loop ((n n) (xs xs) (ys '()))
    (if (or (zero? n) (null? xs))
	(values (reverse ys) xs)
	(loop (- n 1) (cdr xs) (cons (car xs) ys)))))

(define (take-while pred? xs)
  (let loop ((xs xs) (ys '()))
    (if (or (null? xs) (not (pred? (car xs))))
	(reverse ys)
	(loop (cdr xs) (cons (car xs) ys)))))

(define (drop-while pred? xs)
  (let loop ((xs xs))
    (if (or (null? xs) (not (pred? (car xs))))
	xs
	(loop (cdr xs)))))

(define (split-while pred? xs)
  (let loop ((xs xs) (ys '()))
    (if (or (null? xs) (not (pred? (car xs))))
	(values (reverse ys) xs)
	(loop (cdr xs) (cons (car xs) ys)))))

(define (foldl op base xs)
  (if (null? xs)
      base
      (foldl op (op base (car xs)) (cdr xs))))

(define (foldr op base xs)
  (if (null? xs)
      base
      (op (car xs) (foldr op base (cdr xs)))))

(define (filter pred? xs)
  (let loop ((xs xs) (ys '()))
    (if (null? xs)
	(reverse ys)
	(loop (cdr xs) (if (pred? (car xs)) (cons (car xs) ys) ys)))))

(define (remove pred? xs)
  (let loop ((xs xs) (ys '()))
    (if (null? xs)
	(reverse ys)
	(loop (cdr xs) (if (pred? (car xs)) ys (cons (car xs) ys))))))

(define (all? pred? xs)
  (cond
   ((null? xs) #t)
   ((pred? (car xs)) (all? pred? (cdr xs)))
   (else #f)))

(define (any? pred? xs)
  (cond
   ((null? xs) #f)
   ((pred? (car xs)) #t)
   (else (any? pred? (cdr xs)))))

(define (flatten xs)
  (cond
   ((null? xs) xs)
   ((pair? xs) (append (flatten (car xs)) (flatten (cdr xs))))
   (else (list xs))))

(define (zip . xss)
  (apply map list xss))

(define (range . args)
  (case (length args)
    ((1) (range 0 (car args) (if (negative? (car args)) -1 1)))
    ((2) (range (car args) (cadr args) (if (< (car args) (cadr args)) 1 -1)))
    ((3) (let ((le? (if (negative? (caddr args)) >= <=)))
	   (let loop ((x (car args)) (xs '()))
	     (if (le? (cadr args) x)
		 (reverse xs)
		 (loop (+ x (caddr args)) (cons x xs))))))))

(define (cross . xss)
  (define (f xs yss)
    (define (g x zss)
      (define (h ys uss)
        (cons (cons x ys) uss))
      (foldr h zss yss))
    (foldr g '() xs))
  (foldr f (list '()) xss))
