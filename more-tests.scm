(writeln '(1 2 3 yes we do serialize ok! (o . -)))
(writeln '<%%!!weird-=-symbol??%%>)

(writeln (if (eq? () ()) 'ok '(nil != nil wtf)))
(writeln (if (eq? () (cdr '(test))) 'ok '(nil != nil again)))

(writeln (if (eq? 'hey 'hey) 'ok '(symbol equality broken)))
(writeln (if (eq? 'hey 'ho) '(symbol equality broken) 'ok))

(writeln (if (eq? 5 (+ 2 3)) 'ok '(eq sucks)))
(writeln (if (eq? #t (null? '())) 'ok '(eq or null sucks)))
(writeln (if (eq? #f (pair? '())) 'ok '(eq or pair sucks)))

(writeln (if (null? ()) 'ok '(null bad)))
(writeln (if (null? #f) '(false aint null) 'ok))

(writeln (if (pair? (cons 2 3)) 'ok '(pair bad)))
(writeln (if (pair? '(hello)) 'ok '(pair bad)))
(writeln (if (pair? '(h e l l o)) 'ok '(pair bad)))
(writeln (if (pair? '(h e l l . o)) 'ok '(pair bad)))
(writeln (if (pair? '()) '(pair bad) 'ok))
(writeln (if (pair? (* 2 3)) '(pair bad) 'ok))
(writeln (if (pair? #t) '(pair bad) 'ok))
(writeln (if (pair? (eq? 2 3)) '(pair bad) 'ok))

(writeln (if (number? 23) 'ok '(number bad)))
(writeln (if (number? (* 2 3)) 'ok '(number bad)))
(writeln (if (number? (/ 1 0)) 'ok '(number not great)))
(writeln (if (number? 'a) '(number bad) 'ok))
(writeln (if (number? '()) '(number bad) 'ok))
(writeln (if (number? #f) '(number bad) 'ok))
(writeln (if (number? #t) '(number bad) 'ok))

(writeln (if (eq? (((lambda (sq) (lambda (*) (sq 5)))
                        (lambda (x) (* x x)))
                       (lambda (a b) (+ a b))) 25)
                 'ok
                 '(scoping problemz?)))

(define (Z f) ((lambda (R) (R R)) (lambda (x) (lambda () (f (x x))))))
(define ! ((Z (lambda (f) (lambda (n) (if (eq? n 0) 1 (* n ((f) (- n 1)))))))))
(writeln (if (eq? (! 5) 120) 'grrreat '(lost in zee)))

(define (f a b) x)
(define x 0)
(writeln (if (eq? (f (set! x (+ x 1)) (set! x (+ x x))) 1)
                 '(right to left)
                 '(left to right)))

(define (shmota n acc) (if (= n 0) acc (shmota (- n 1) (cons n acc))))
(define (iota n) (shmota n '()))
(writeln (iota 1000)) ;;; with 10k it fails ofc

(define (map f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs)))))
(writeln (map (lambda (x) (* x x)) (iota 10)))

(define (mk-silly n) (lambda (x) (cons x n)))
(writeln (map (lambda (f) (f 'const)) (map mk-silly (iota 5)))) ;; gr8!

(define x 23)
(define (foo x) ((lambda (_) (* x x)) (set! x 10)))
(writeln '(expecting 100:))
(writeln (foo x))
(writeln '(expecting 23:))
(writeln x)

(define x 23)
(define (boo y) ((lambda (_) (* y y)) (set! x 10)))
(writeln '(expecting 529:))
(writeln (boo x))
(writeln '(expecting 10:))
(writeln x)

(writeln ((if #t + *) 2 3))
(writeln ((if #f + *) 2 3))
(writeln (if '() 'ok 'nope))
(writeln (if '(woo) 'ok 'nope))

(writeln (eq? 'lambda (car ((lambda lambda lambda) 'lambda)))) ;; niiice!
;;(writeln ((lambda (if) (if 'p 'c 'a)) (lambda (a b c) (cons a (cons b (cons c '())))))) ;; oww, to-js needs to be aware of bound symbols then...

(writeln '(now something really stupid, overwrite + with *))
(define old-+ +)
(set! + *)
(writeln `((+ 2 3) is now ,(+ 2 3)))
(set! + old-+)
(writeln `(and now (+ 2 3) is ,(+ 2 3) again))

(define evil 13)
(writeln `(13 is ,evil))
(define evil 23)
(writeln `(23 is ,evil))

(define (foo x) (* x x))
(writeln '(define (foo x) (* x x)))
(writeln `((foo 3) is ,(foo 3)))

(define (foo x) (+ x x))
(writeln '(define (foo x) (+ x x)))
(writeln `(and now (foo 3) is ,(foo 3)))

(writeln
 ((lambda ()
    (define (sq x) (* x x))
    (define x 5)
    (sq x))))

(writeln
 `(expecting 9 here: 
	     ,((lambda (x)
		 (writeln 'ple-ple-ple)
		 (define (skwyr x) (* x x))
		 (define (ole! y) (set! x y) y)
		 (ole! (skwyr x))
		 x) (+ 2 1))))

(begin
  (writeln '(just some silly begin stuff))
  (define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))
  (writeln `((fac 5) is ,(fac 5)))
  (define more-than-100 (fac 5))
  (writeln `((and this should be sth around 14400:) ,(* more-than-100 more-than-100))))

(writeln '(expecting (5 25 (5 5) (25 25)) below:))
(writeln
 (let* ((x (+ 2 3))
        (y (* x x)))
   (define (dup x) (list x x))
   (list x y (dup x) (dup y))))

(begin
  (define leaky-var 23)
  (writeln `(leaky-var is ,leaky-var inside begin)))

(writeln `(leaky-var is ,leaky-var outside begin too))

(define (stupid x) (begin (writeln 'makes-no-sense-but-works) x))
(writeln (stupid (+ 2 3)))
