(define (ack m n)
  (if (= m 0)
      (+ n 1) 
      (if (= n 0)
          (ack (- m 1) 1)
          (ack (- m 1) (ack m (- n 1))))))

(define (rep n f)
  (if (= n 0)
      #t
      (begin (f) (rep (- n 1) f))))

(rep 100 (lambda () (writeln (ack 3 10))))
