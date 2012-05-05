(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((c (read-char)) (cs '()))
	(if (eof-object? c)
	    (reverse cs)
	    (loop (read-char) (cons c cs)))))))

(define (read-line . port)
  (define (eat p c)
    (if (and (not (eof-object? (peek-char p)))
	     (char=? (peek-char p) c))
	(read-char p)))
  (let ((p (if (null? port) (current-input-port) (car port))))
    (let loop ((c (read-char p)) (line '()))
      (cond ((eof-object? c) (if (null? line) c (list->string (reverse line))))
	    ((char=? #\newline c) (eat p #\return) (list->string (reverse line)))
	    ((char=? #\return c) (eat p #\newline) (list->string (reverse line)))
	    (else (loop (read-char p) (cons c line)))))))
