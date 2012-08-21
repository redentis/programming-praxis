; constructor function for tree nodes
(define (tree k v l r)
  (vector k v l r (+ (max (height l) (height r)) 1)))

; define the invariant nil node
(define nil
  (vector 'nil 'nil 'nil 'nil' 0))

(define (key t)
  (vector-ref t 0))

(define (value t)
  (vector-ref t 1))

(define (left t)
  (vector-ref t 2))

(define (right t)
  (vector-ref t 3))

(define (height t)
  (vector-ref t 4))

(define (nil? t)
  (equal? t nil))

(define (weight t)
  (- (height (left t)) (height (right t))))

; rotate functions required for balancing the tree
(define (balance t)
  (let ((b (weight t)))
    (cond ((and (= b 2) (= (weight (left t)) -1))
	   (rotate-right (tree (key t) (value t) (rotate-left (left t)) (right t))))
	  ((= b 2) (rotate-right t))
	  ((and (= b -2) (= (weight (right t)) 1))
	   (rotate-left (tree (key t) (value t) (left t) (rotate-right (right t)))))
	  ((= b -2) (rotate-left t))
	  (else t))))

(define (rotate-left t)
  (if (nil? t)
      t
      (tree (key (right t))
	    (value (right t))
	    (tree (key t) (value t) (left t) (left (right t)))
	    (right (right t)))))

(define (rotate-right t)
  (if (nil? t)
      t
      (tree (key (left t))
	    (value (left t))
	    (left (left t))
	    (tree (key t) (value t) (right (left t)) (right t)))))

; lookup key in a tree
(define (lookup lt? t k)
  (cond ((nil? t) #f)
	((lt? k (key t)) (lookup lt? (left t) k))
	((lt? (key t) k) (lookup lt? (right t) k))
	(else (cons k (value t)))))

(define (insert lt? t k v)
  (cond ((nil? t) (tree k v nil nil))
	((lt? k (key t)) (balance (tree (key t) (value t) (insert lt? (left t) k v) (right t))))
	((lt? (key t) k) (balance (tree (key t) (value t) (left t) (insert lt? (right t) k v))))
	(else (tree k v (left t) (right t)))))

(define (delete lt? t k)
  (define (delete-successor t)
    (cond ((nil? (left t)) (values (right t) (key t) (value t)))
	  (else (call-with-values
		    (lambda () (delete-successor (left t)))
		  (lambda (l k v) (values (balance (tree (key t) (value t) l (right t))) k v))))))
  (let delete ((t t))
    (cond ((nil? t) nil)
	  ((lt? k (key t)) (balance (tree (key t) (value t) (delete (left t)) (right t))))
	  ((lt? (key t) k) (balance (tree (key t) (value t) (left t) (delete (right t)))))
	  ((nil? (left t)) (right t))
	  ((nil? (right t)) (left t))
	  (else (call-with-values
		    (lambda () (delete-successor (right t)))
		    (lambda (r k v) (balance (tree k v (left t) r))))))))

; Return an in-order list of key-value pairs in the given tree.
(define (avl->list t)
  (cond ((nil? t) '()) ; terminal node
	((and (nil? (left t)) (nil? (right t))) (list (cons (key t) (value t)))) ; leaf node
	(else (append (avl->list (left t)) (list (cons (key t) (value t))) (avl->list (right t)))))) ; branch node

; Build an AVL tree from a given list. The list elements are assumed to be key-value pairs.
(define (list->avl lt? l)
  (let loop ((t nil) (l l))
    (if (null? l)
	t
	(loop (insert lt? t (caar l) (cdar l)) (cdr l)))))


; utility functions

(define (display-avl t . indent)
  (let ((indent (if (null? indent) "" (car indent))))
    (if (nil? t)
	(begin (display indent) (display "nil nil 0") (newline))
	(begin (display indent) (display (key t))
	       (display " ") (display (value t))
	       (display " ") (display (height t))
	       (newline)
	       (display-avl (left t) (string-append " " indent))
	       (display-avl (right t) (string-append " " indent))))))

; test that a given object is valid AVL tree
(define (check? lt? t)
  (if (nil? t) #t
    (and (or (nil? (left t)) (lt? (key (left t)) (key t)))
         (or (nil? (right t)) (lt? (key t) (key (right t))))
         (< (abs (weight t)) 2)
         (check? lt? (left t))
         (check? lt? (right t)))))
