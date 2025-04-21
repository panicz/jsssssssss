(console.log (if (eq? () ()) 'ok '(nil != nil wtf)))
(console.log (if (eq? () (cdr '(test))) 'ok '(nil != nil again)))

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

(define (map f xs) (if (eq? xs '()) '() (cons (f (car xs)) (map f (cdr xs)))))
(console.log (map (lambda (x) (* x x)) (iota 10)))

(define (mk-silly n) (lambda (x) (cons x n)))
(console.log (map (lambda (f) (f 'const)) (map mk-silly (iota 5)))) ;; gr8!

(define x 23)
(define (foo x) ((lambda (_) (* x x)) (set! x 10)))
(console.log '(expecting 100:))
(console.log (foo x))
(console.log '(expecting 23:))
(console.log x)

(define x 23)
(define (boo y) ((lambda (_) (* y y)) (set! x 10)))
(console.log '(expecting 529:))
(console.log (boo x))
(console.log '(expecting 10:))
(console.log x)

(console.log ((if #t + *) 2 3))
(console.log ((if #f + *) 2 3))
