;; http://programmingpraxis.com/2009/03/23/binary-search/

(define (println . args)
  (let loop ((args args))
    (if (null? args)
	(newline)
	(begin
	  (display (car args))
	  (loop (cdr args))))))

;; assumption is that the array of ns to search is already sorted and in non-descending order
(define (bsearch n ns)
  (if (or (null? ns)
	  (= 0 (vector-length ns)))
      #f
      (let loop ((lower 0)
		 (upper (- (vector-length ns) 1)))
	(let* ((i (+ lower (floor (/ (- upper lower) 2))))
	       (p (vector-ref ns i)))
;	  (println lower "," upper)
	  (cond ((= n p) i)
		((= lower upper) #f)
		((< n p) (loop lower (- i 1)))
		(else (loop (+ i 1) upper)))))))
