(define (make-matrix rows cols . value)
  (do ((m (make-vector rows)) (i 0 (+ i 1)))
      ((= i rows) m)
    (if (null? value)
	(vector-set! m i (make-vector cols))
	(vector-set! m i (make-vector cols (car value))))))

(define (matrix-rows m) (vector-length m))

(define (matrix-cols m) (vector-length (vector-ref m 0)))

(define (matrix-ref m i j) (vector-ref (vector-ref m i) j))

(define (matrix-set! m i j x) (vector-set! (vector-ref m i) j x))

(define-syntax for
  (syntax-rules ()
    ((_ (var first past step) body ...)
     (let ((ge? (if (< first past) >= <=)))
       (do ((var first (+ var step)))
	   ((ge? var past))
	 body ...)))
    ((_ (var first past) body ...)
     (let* ((f first) (p past) (s (if (< first past) 1 -1)))
       (for (var f p s) body ...)))
    ((_ (var past) body ...)
     (let* ((p past) (for (var 0 p) body ...))))))

(define (matrix-add m n)
)
