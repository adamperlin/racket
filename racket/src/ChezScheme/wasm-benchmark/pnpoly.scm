;;; PNPOLY - Test if a point is contained in a 2D polygon.
  
(import (rnrs base)
        (rnrs io simple)
        (rnrs arithmetic flonums))

(include "benchmark-common.scm")

(define (pt-in-poly2 xp yp x y)
  (let loop ((c #f) (i (- (vector-length xp) 1)) (j 0))
       ; (printf "i is: ~a\n" i)
    (if (< i 0)
      c
      (if (or (and (or (fl>? (vector-ref yp i) y)
                       (fl>=? y (vector-ref yp j)))
                   (or (fl>? (vector-ref yp j) y)
                       (fl>=? y (vector-ref yp i))))
              (fl>=? x
                       (fl+ (vector-ref xp i)
                               (fl/ (fl*
                                        (fl- (vector-ref xp j)
                                                (vector-ref xp i))
                                        (fl- y (vector-ref yp i)))
                                       (fl- (vector-ref yp j)
                                               (vector-ref yp i))))))
        (loop c (- i 1) i)
        (loop (not c) (- i 1) i)))))

(define (run input1 input2)
  (let ((count 0)
        (xp (list->vector (vector->list input1)))
        (yp (list->vector (vector->list input2))))
    (if (pt-in-poly2 xp yp .5 .5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 1.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 1.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .75 2.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp 0. 2.01) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 2.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -1. -.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -1.5 .5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -2.25 -1.) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 -.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 -1.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 -2.5) (set! count (+ count 1)))
    count))

(define (pnpoly-main . args)
  (let* ((count (car args))
         (input1 (cadr args))
         (input2 (caddr args))
         (output (cadddr args))
         (s2 (number->string count))
         (s1 "")
         (name "pnpoly"))
    (run-r6rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (run (hide count input1) (hide count input2)))
     (lambda (result) (and (number? result) (= result output))))))

; (main 500000
;         '#(0. 1. 1. 0. 0. 1. -.5 -1. -1. -2. -2.5 -2. -1.5 -.5 1. 1. 0. -.5 -1. -.5)
;         '#(0. 0. 1. 1. 2. 3. 2. 3. 0. -.5 -1.  -1.5 -2. -2. -1.5 -1. -.5 -1. -1. -.5) 
;         6)
