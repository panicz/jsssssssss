(console.log '(1 2 3 yes we do serialize ok! (o . -)))
(console.log '<%%!!weird-=-symbol??%%>)

(console.log (if (eq? () ()) 'ok '(nil != nil wtf)))
(console.log (if (eq? () (cdr '(test))) 'ok '(nil != nil again)))

(console.log (if (eq? 'hey 'hey) 'ok '(symbol equality broken)))
(console.log (if (eq? 'hey 'ho) '(symbol equality broken) 'ok))

(console.log (if (eq? 5 (+ 2 3)) 'ok '(eq sucks)))
(console.log (if (eq? #t (null? '())) 'ok '(eq or null sucks)))
(console.log (if (eq? #f (pair? '())) 'ok '(eq or pair sucks)))

(console.log (if (null? ()) 'ok '(null bad)))
(console.log (if (null? #f) '(false aint null) 'ok))

(console.log (if (pair? (cons 2 3)) 'ok '(pair bad)))
(console.log (if (pair? '(hello)) 'ok '(pair bad)))
(console.log (if (pair? '(h e l l o)) 'ok '(pair bad)))
(console.log (if (pair? '(h e l l . o)) 'ok '(pair bad)))
(console.log (if (pair? '()) '(pair bad) 'ok))
(console.log (if (pair? (* 2 3)) '(pair bad) 'ok))
(console.log (if (pair? #t) '(pair bad) 'ok))
(console.log (if (pair? (eq? 2 3)) '(pair bad) 'ok))

(console.log (if (number? 23) 'ok '(number bad)))
(console.log (if (number? (* 2 3)) 'ok '(number bad)))
(console.log (if (number? (/ 1 0)) 'ok '(number not great)))
(console.log (if (number? 'a) '(number bad) 'ok))
(console.log (if (number? '()) '(number bad) 'ok))
(console.log (if (number? #f) '(number bad) 'ok))
(console.log (if (number? #t) '(number bad) 'ok))

(console.log (if (eq? (((lambda (sq) (lambda (*) (sq 5)))
                        (lambda (x) (* x x)))
                       (lambda (a b) (+ a b))) 25)
                 'ok
                 '(scoping problemz?)))

(define (Z f) ((lambda (R) (R R)) (lambda (x) (lambda () (f (x x))))))
(define ! ((Z (lambda (f) (lambda (n) (if (eq? n 0) 1 (* n ((f) (- n 1)))))))))
(console.log (if (eq? (! 5) 120) 'grrreat '(lost in zee)))

(define (f a b) x)
(define x 0)
(console.log (if (eq? (f (set! x (+ x 1)) (set! x (+ x x))) 1)
                 '(right to left)
                 '(left to right)))

(define (shmota n acc) (if (= n 0) acc (shmota (- n 1) (cons n acc))))
(define (iota n) (shmota n '()))
(console.log (iota 1000)) ;;; with 10k it fails ofc

(define (map f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs)))))
(console.log (map (lambda (x) (* x x)) (iota 10)))

(define (mk-silly n) (lambda (x) (cons x n)))
(console.log (map (lambda (f) (f 'const)) (map mk-silly (iota 5)))) ;; gr8!

(define (list . x) x)

(define x 23)
(define (foo x) ((lambda (_) (* x x)) (set! x 10)))
(console.log (list 'expecting 100 'here: (foo x)))
(console.log (list 'expecting 23 'here: x))

(define x 23)
(define (boo y) ((lambda (_) (* y y)) (set! x 10)))
(console.log (list 'expecting 529 'here: (boo x)))
(console.log (list 'and 10 'here: x))

(console.log (list 'expecting 5 'here: ((if #t + *) 2 3)))
(console.log (list 'expecting 6 'here: ((if #f + *) 2 3)))
(console.log (if '() 'if-is-ok 'nope))
(console.log (if '(woo) 'if-is-ok 'nope))

(console.log (if (eq? 'lambda (car ((lambda lambda lambda) 'lambda))) 'nice 'boo))
;;(console.log ((lambda (if) (if 'p 'c 'a)) (lambda (a b c) (cons a (cons b (cons c '())))))) ;; oww, to-js needs to be aware of bound symbols then...

(console.log '(now something really stupid, overwrite + with *))
(define old-+ +)
(set! + *)

(console.log (list '(+ 2 3) 'is 'now (+ 2 3)))
(set! + old-+)
(console.log (list 'and 'now '(+ 2 3) 'is (+ 2 3) 'again))

(define evil 13)
(console.log (list 13 'is evil))
(define evil 23)
(console.log (list 23 'is evil))

(define (foo x) (* x x))
(console.log '(define (foo x) (* x x)))
(console.log (list '(foo 3) 'is (foo 3)))

(define (foo x) (+ x x))
(console.log '(define (foo x) (+ x x)))
(console.log (list 'and 'now '(foo 3) 'is (foo 3)))

(console.log '(expecting 25 below:))
(console.log
 ((lambda ()
    (define (sq x) (* x x))
    (define x 5)
    (sq x))))

(console.log
 (list 'expecting 9 'here:
       ((lambda (x)
          (console.log 'ple-ple-ple)
          (define (skwyr x) (* x x))
          (define (ole! y) (set! x y) y)
          (ole! (skwyr x))
          x) (+ 2 1))))

(begin
  (console.log '(just some silly begin stuff))
  (define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))
  (console.log (list '(fac 5) 'is (fac 5)))
  (define more-than-100 (fac 5))
  (console.log (list '(and this should be sth around 14400:) (* more-than-100 more-than-100))))

(console.log '(expecting (5 25 (5 5) (25 25)) below:))
(console.log
 (let* ((x (+ 2 3))
        (y (* x x)))
   (define (dup x) (list x x))
   (list x y (dup x) (dup y))))

(begin
  (define leaky-var 23)
  (console.log (list 'leaky-var 'is leaky-var 'inside 'begin)))

(console.log (list 'leaky-var 'is leaky-var 'outside 'begin 'too))

(define (stupid x) (begin (console.log 'makes-no-sense-but-works) x))
(stupid (+ 2 3))

(define (evil x) (console.log 'elo) (begin (set! x 7) (* x x)))
(console.log (list 'expecting 49 'here (evil 3)))

(define (evil* x) (console.log 'elo) (begin (set! x 7) (* x x)) (console.log 'hahaha) 42)
(console.log (list 'expecting 42 'here (evil* 3)))

(define (fooo)
  (begin (define x (+ 2 3)) ;; nb Scheme48 doesn't like this (syntax violation)
         (define y (* x x))
         (cons x y)))

(console.log (list 'expecting '(5 . 25) 'here (fooo)))

(define (fooooo)
  (console.log 'even-better?)
  (begin (define x (+ 2 3))
         (define y (* x x))
         (cons x y)))

(console.log (list 'expecting '(5 . 25) 'here 'as 'well (fooooo)))
