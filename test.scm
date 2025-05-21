;;; (begin-tests "basic tests")

(cond-expand
 (guile
  (use-modules (base)))
 (else))

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(e.g. (factorial 5) ===> 120)

(e.g. (apply + 1 2 '(3 4 5)) ===> 15)
(e.g. (apply + '(1 2 3)) ===> 6)

(define (counter n)
  (lambda ()
    (set! n (+ n 1))
    n))

(define c (counter 0))

(e.g. (c) ===> 1)
(e.g. (c) ===> 2)
(e.g. (c) ===> 3)

(define p (make-parameter 0))

(e.g. (p) ===> 0)

(parameterize ((p 10))
  (e.g. (p) ===> 10)
  (p (+ (p) 1))
  (e.g. (p) ===> 11))

(e.g. (p) ===> 0)

(e.g.
 (call-with-output-string
   (lambda (p)
     (write-char #\d p)
     (write-char #\u p)
     (write-char #\p p)
     (write-char #\a p))) ===> "dupa")

(e.g.
 (with-output-to-string
   (lambda ()
     (write-char #\d)
     (write-char #\u)
     (write-char #\p)
     (write-char #\a))) ===> "dupa")

(e.g.
 (call-with-input-string "dupa"
   (lambda (input)
     (list->string
      (let* ((d (read-char input))
	     (u (read-char input))
	     (p (read-char input))
	     (a (read-char input)))
	(list d u p a))))) ===> "dupa")

(e.g.
 (with-input-from-string "dupa"
   (lambda ()
     (list->string
      (let* ((d (read-char))
	     (u (read-char))
	     (p (read-char))
	     (a (read-char)))
	(list d u p a))))) ===> "dupa")

(writeln (current-output-port))
(writeln current-output-port)
(writeln "a'\"\\b")

(e.g. (string->list "dupa") ===> (#\d #\u #\p #\a))

(e.g.
 (with-output-to-string
   (lambda ()
     (for-each write-char (string->list "dupa")))) ===> "dupa")
 

(e.g.
 (with-output-to-string
   (lambda ()
     (with-input-from-string "dupa"
       (lambda ()
	 (while (isnt (peek-char) eof-object?)
	   (write-char (read-char))))))) ===> "dupa")

(e.g. (/ 1.0 0.0) ===> +inf.0)
(e.g. (/ -1.0 0.0) ===> -inf.0)
(e.g. (nan? (/ 0.0 0.0)))
(e.g. (/ 1.0 2.0) ===> 0.5)
(e.g. (/ 2.0 2.0 2.0 2.0) ===> 0.25)

(e.g. (map + '(1 2 3) '(4 5 6)) ===> (5 7 9))

(e.g. (map char->integer (string->list "dupa"))
      ===> (100 117 112 97))

(e.g.
 (list->string
  (map integer->char '(100 117 112 97))) ===> "dupa")

(e.g. (string-take "dupa" 2) ===> "du")

(e.g. (string-drop "dupa" 2) ===> "pa")

(e.g. (number->string 12) ===> "12")

(e.g. (string->number "34") ===> 34)

(with-input-from-string "dupa"
  (lambda ()
    (while (isnt (read-char) eof-object?))))

(e.g.
 (let ((s "dupa"))
   (map (lambda (i) (string-ref s i)) '(0 1 2 3)))
 ===> (#\d #\u #\p #\a))

(define A '(1 2 3))

(define B (append A '(4 5 6) '(7 8 9)))

(e.g. A ===> (1 2 3))
(e.g. B ===> (1 2 3 4 5 6 7 8 9))
(e.g. (isnt A eq? B))

(cond-expand
 (jsssssssss
  (define a '(1 2 3))

  (define b (append! a '(4 5 6) '(7 8 9)))

  (e.g. a ===> (1 2 3 4 5 6 7 8 9))
  (e.g. b ===> (1 2 3 4 5 6 7 8 9))
  (e.g. (eq? a b))

  (e.g. (isnt b eq? B))
  (e.g. (equal? b B))
  
  (e.g. (string-match "^[0-9]+$" "123"))

  (writeln (string-match "^[0-9]+$" "dupa"))

  (e.g. (append! '(a b c) 'd) ===> (a b c . d))

  (e.g.
   (catch (lambda (e) (slot-ref e 'message))
     (e.g. (+ 2 2) ===> 5)) ===> "while evaluating

  (+ 2 2)

expected:

  5

got:

  4
")

  
  )
 (else))

(e.g.
 (begin
   (when (file-exists? "dupa.txt")
     (delete-file "dupa.txt"))
   
   (with-output-to-file "dupa.txt"
     (lambda ()
       (for-each write-char (string->list "dupa"))))

   (let ((dupa (with-output-to-string
		 (lambda ()
		   (with-input-from-file "dupa.txt"
		     (lambda ()
		       (while (isnt (peek-char) eof-object?)
			 (write-char (read-char)))))))))
     (delete-file "dupa.txt")
     dupa)) ===> "dupa")


(writeln
 (union '(a b c) '(a c e)))

(writeln
 (difference '(a b c) '(a c e)))


;; the following test only runs when invoking
;;
;;   $ ./jsssssssss.scm < test.scm > test.js
;;   $ node test.js
;;
;; and breaks when invoked as
;;
;;   $ ./jsssssssss.scm < test.scm | node
;;
;; (which isn't that bad after all)

(writeln "type any key and press enter:")
(let ((c (read-char)))
  (writeln "you typed: " c))

;;(end-tests)
