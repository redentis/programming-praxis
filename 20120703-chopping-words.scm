;; http://programmingpraxis.com/2012/07/03/chopping-words/
;; A simple word game starts with a word and repeatedly removes a single letter from the word,
;; forming a new word, until there are no letters left. The act of removing a letter is called
;; chopping and the resulting list of words is a chopping chain.

(load "prelude/avl-trees.scm") ; required for the dictionary implementation
(load "prelude/higher-order-functions.scm") ; required for partial

(define (mappend f . args)
  "Implementation of map that uses append rather than cons to build up the return value"
  (apply append (apply map f args)))

(define (variations word? w)
  "For the given word, return a list of variations arrived at by removing a single letter and checking with the given word? function whether this is a valid word or not."
  (let ((len (string-length w)))
    (if (< len 1)
	'()
	(let loop ((i 0) (result '()))
	  (if (= i len)
	      result
	      (let ((v (string-append (substring w 0 i) (substring w (+ i 1) len))))
		(loop (+ i 1) (if (word? v) (cons v result) result))))))))


(define (chains w)
  (let loop ((chains (list (list w))))
    (if (= (string-length (caar chains)) 1)
	(map reverse chains)
	(loop (mappend
	       (lambda (words)
		 (map (lambda (word) (cons word words)) (dictionary-variations (car words))))
	       chains)))))

;; set up the test for "planet"
(define (build-tree words)
  (let loop ((words words) (result nil))
    (if (null? words)
	result
	(loop (cdr words) (insert string-ci< result (car words) #t)))))

(define dictionary (build-tree '("planet" "plant" "plane" "plan" "pant" "pan" "ant" "an" "pa" "a")))

(define (dictionary-word? w) (lookup string-ci< dictionary w))

(define dictionary-variations (partial variations dictionary-word?))
