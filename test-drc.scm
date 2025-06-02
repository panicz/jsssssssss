#!/usr/bin/guile \
-L . -s
!#
(cond-expand
 (guile
  (import (base))
  (define (writeln x) (write x) (newline) x))
 (else))

(define (forget n D)
  (match D
    ('() '())
    (`((,k . ,v) . ,D*) (if (eq? k n)
                            D*
                            `((,k . ,v) . ,(forget n D*))))))

(define (lookup n D)
  (match D
    ('() 'UNKNOWN-NAME)
    (`((,k . ,v) . ,D*) (if (eq? k n)
                            v
                            (lookup n D*)))))

(define (=>* d r c)
;  (writeln `(D: ,d))
;  (writeln `(R: ,r))
;  (writeln `(C: ,c))
;  (writeln '---)
  (match `(,d ,r ,c)

    (`(,D (,e . ,R) ((NAME ,n) . ,C))   (=>* `((,n . ,e) . ,D) R C))
    (`(,D    ,R     ((FORGET ,n) . ,C)) (=>* (forget n D) R C))
    (`(,D    ,R     ((LOOKUP ,n) . ,C)) (=>* D `(,(lookup n D) . ,R) C))

    (`(,D ,R ((CONST ,e) . ,C)) (=>* D `(,e . ,R) C))
    (`(,D ,R ((PROC ,p)  . ,C)) (=>* D `(,p . ,R) C))

    (`(,D ((,h . ,t) . ,R) ((CAR) . ,C)) (=>* D `(,h . ,R) C))
    (`(,D ((,h . ,t) . ,R) ((CDR) . ,C)) (=>* D `(,t . ,R) C))
    (`(,D (,h ,t . ,R) ((CONS) . ,C)) (=>* D `((,h . ,t) . ,R) C))
    (`(,D (,n ,m . ,R) ((PLUS) . ,C)) (=>* D `(,(+ n m) . ,R) C))
    (`(,D (,n ,m . ,R) ((MINUS) . ,C)) (=>* D `(,(- n m) . ,R) C))
    (`(,D (,n ,m . ,R) ((TIMES) . ,C)) (=>* D `(,(* n m) . ,R) C))

    ;(`(,D (,x ,x . ,R) ((EQ?) . ,C)) (=>* D `(T . ,R) C))
    ;(`(,D (,x ,y . ,R) ((EQ?) . ,C)) (=>* D `(() . ,R) C))
    (`(,D (,x ,y . ,R) ((EQ?) . ,C))  (=>* D `(,(if (equal? x y) 'T '()) . ,R) C))

    (`(,D ((,h . ,t) . ,R) ((ATOM?) . ,C)) (=>* D `(() . ,R) C))
    (`(,D (   ,a     . ,R) ((ATOM?) . ,C)) (=>* D `(T . ,R) C))
    (`(,D ((? number?) . ,R) ((NUM?) . ,C)) (=>* D `(T . ,R) C))
    (`(,D (    ,x      . ,R) ((NUM?) . ,C)) (=>* D `(() . ,R) C))
    (`(,D ((? symbol?) . ,R) ((SYM?) . ,C)) (=>* D `(T . ,R) C))
    (`(,D (    ,x      . ,R) ((SYM?) . ,C)) (=>* D `(() . ,R) C))

    (`(,D (() . ,R) ((SELECT ,p ,p*) . ,C)) (=>* D R `(,@p* ,@C)))
    (`(,D ( T . ,R) ((SELECT ,p ,p*) . ,C)) (=>* D R `(,@p ,@C)))

    (`(,D (,p . ,R) ((APPLY) . ,C)) (=>* D R `(,@p ,@C)))

    (`(,D (,r . ,R) ()) r)))

(define p1
  '((PROC ((NAME xs)
           (NAME ys)
           (LOOKUP xs)
           (CONST ())
           (EQ?)
           (SELECT ((LOOKUP ys))
                   ((LOOKUP ys)
                    (LOOKUP xs)
                    (CDR)
                    (LOOKUP apd)
                    (APPLY)
                    (LOOKUP xs)
                    (CAR)
                    (CONS)))
           (FORGET ys)
           (FORGET xs)))
    (NAME apd)
    (LOOKUP apd)
    (APPLY)
    (LOOKUP apd)
    (APPLY)
    (LOOKUP apd)
    (APPLY)
    (LOOKUP apd)
    (APPLY)))

(define (DRC program data) (=>* '() data program))

(define (shmota n acc) (if (= n 0) acc (shmota (- n 1) (cons n acc))))
(define (iota n) (shmota n '()))

(define (map1 f xs)
  (if (null? xs) '() (cons (f (car xs)) (map1 f (cdr xs)))))

(map1 (lambda (_)
       (DRC p1 '((m o r) (d e r c z a) (m a) (k u l a) (t u r a))))
     (iota 10000))
