(define-syntax fold-of
  (syntax-rules (range in is)
    ((_ "z" f b e) 
     (set! b (f b e)))
    ((_ "z" f b e (v range first past stop) c ...)
     (let* ((x first) (p past) (s stop)
	    (le? (if (positive? s) <= >=)))
       (do ((v x (+ v s))) ((le? p v) b)
	 (fold-of "z" f b e c ...))))
    ((_ "z" f b e (v range first past) c ...)
     (let* ((x first) (p past) (s (if (< x p) 1 -1)))
       (fold-of "z" f b e (v range x p s) c ...)))
    ((_ "z" f b e (v range past) c ...)
     (fold-of "z" f b e (v range 0 past) c ...))
    ((_ "z" f b e (x in xs) c ...)
     (do ((t xs (cdr t))) ((null? t) b)
       (let ((x (car t)))
	 (fold-of "z" f b e c ...))))
    ((_ "z" f b e (x is y) c ...)
     (let ((x y)) (fold-of f b e c ...)))
    ((_ "z" f b e p? c ...)
     (if p? (fold-of "z" f b e c ...)))
    ((_ f i e c ...)			; main entry point, the other syntax rules are internal
     (let ((b i)) (fold-of "z" f b e c ...)))))

(define-syntax list-of
  (syntax-rules ()
    ((_ arg ...) (reverse (fold-of (lambda (d a) (cons a d)) '() arg ...)))))

