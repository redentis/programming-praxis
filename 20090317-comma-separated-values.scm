;; http://programmingpraxis.com/2009/03/17/comma-separated-values/

;; Very intersting exercise as this uses a state machine approach where each state is representing using a function.
;; The code is more efficient than it seems because all the function calls should take advantage of tail-call
;; optimisation to avoid potentiall large amounts of recursion.

(define (read-csv-record . args)
  (define (read-csv field-sep port)
    (define (add-field field fields)
      (cons (list->string (reverse field)) fields))
    (define (initial field fields)
      (let ((c (read-char port)))
	(cond ((eof-object? c) fields)
	      ((char=? c #\") (if (null? field) (quoted-field field fields) (initial (cons c field) fields)))
	      ((char=? c field-sep) (initial '() (add-field field fields)))
	      ((or (char=? c #\newline)
		   (char=? c #\return)) (terminal '() (add-field field fields)))
	      (else (unquoted-field (cons c field) fields)))))
    (define (unquoted-field field fields)
      (let ((c (read-char port)))
	(cond ((eof-object? c) (add-field field fields))
	      ((char=? c field-sep) (initial '() (add-field field fields)))
	      ((or (char=? c #\newline)
		   (char=? c #\return)) (terminal '() (add-field field fields)))
	      (else (unquoted-field (cons c field) fields)))))
    (define (quoted-field field fields)
      (let ((c (read-char port)))
	(cond ((eof-object? c) fields)
	      ((char=? c #\") (if (char=? (peek-char port) #\")
				  (quoted-field (cons (read-char port) field) fields)
				  (initial field fields)))
	      (else (quoted-field (cons c field) fields)))))
    (define (terminal field fields)
      (let ((c (peek-char port)))
	(cond ((eof-object? c) fields) ; this must be the first condition evaluated as doing a char=? on #!eof throws an error
	      ((or (char=? c #\newline)
		   (char=? c #\return)) (read-char port) fields)
	      (else fields))))
    (if (eof-object? (peek-char port)) (read-char port) (reverse (initial '() '()))))
  (cond ((null? args) (read-csv #\, (current-input-port))) ; no arguments provided
	((and (null? (cdr args)) (char? (car args))) (read-csv (car args) (current-input-port))) ; a single char argument provided -- specified the field separator
	((and (null? (cdr args)) (port? (car args))) (read-csv #\, (car args))) ; a single port argument provided
	((and (char? (car args)) (port? (cadr args))) (read-csv (car args) (cadr args))) ; two arguments provided -- assume that first is the field separator char and the second is the port
	(else (read-csv #\, (current-input-port))))) ; use the default separator and port if we don't otherwise recognise the arguments

;; test code copied from http://programmingpraxis.com/2009/03/17/comma-separated-values/2/

(define csv-test (string-append
  "1,abc,def ghi,jkl,unquoted character strings\n"
  "2,\"abc\",\"def ghi\",\"jkl\",quoted character strings\n"
  "3,123,456,789,numbers\n"
  "4, abc,def , ghi ,strings with whitespace\n"
  "5, \"abc\",\"def\" , \"ghi\" ,quoted strings with whitespace\n"
  "6, 123,456 , 789 ,numbers with whitespace\n"
  "7,TAB123,456TAB,TAB789TAB,numbers with tabs for whitespace\n"
  "8, -123, +456, 1E3,more numbers with whitespace\n"
  "9,123 456,123\"456, 123 456 ,strange numbers\n"
  "10,abc\",de\"f,g\"hi,embedded quotes\n"
  "11,\"abc\"\"\",\"de\"\"f\",\"g\"\"hi\",quoted embedded quotes\n"
  "12,\"\",\"\" \"\",\"\"x\"\",doubled quotes\n"
  "13,\"abc\"def,abc\"def\",\"abc\" \"def\",strange quotes\n"
  "14,,\"\", ,empty fields\n"
  "15,abc,\"def\n ghi\",jkl,embedded newline\n"
  "16,abc,\"def\",789,multiple types of fields\n"))

(with-input-from-string csv-test (lambda ()
    (do ((csv-record (read-csv-record) (read-csv-record)))
        ((eof-object? csv-record))
      (display (list-ref csv-record 0)) (display "|")
      (display (list-ref csv-record 1)) (display "|")
      (display (list-ref csv-record 2)) (display "|")
      (display (list-ref csv-record 3)) (display "|")
      (display (list-ref csv-record 4)) (newline))))
