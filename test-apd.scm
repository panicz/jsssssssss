(define (apd xs ys)
  (if (null? xs) ys (cons (car xs) (apd (cdr xs) ys))))

(define (apd3 xs ys zs) (apd (apd xs ys) zs))

(define (rev xs sx) (if (null? xs) sx (rev (cdr xs) (cons (car xs) sx))))
(define (reverse xs) (rev xs '()))

(define (shmota n) (if (= n 0) '(0) (cons n (shmota (- n 1)))))
(define (iota n) (reverse (shmota (- n 1))))

(define (map1 f xs) (if (null? xs) '() (cons (f (car xs)) (map1 f (cdr xs)))))

(define (jedziemy n m)
  (map1 (lambda (_)
         (apd3 (iota m) (iota m) (iota m)))
       (iota n)))

(jedziemy 1000 1000)
