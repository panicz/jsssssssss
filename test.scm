(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(console.log (factorial 5))

(console.log (apply + 1 2 '(3 4 5)))
(console.log (apply + '(1 2 3)))

(define (counter n)
  (lambda ()
    (set! n (+ n 1))
    n))

(define c (counter 0))

(console.log (c))
(console.log (c))
(console.log (c))
