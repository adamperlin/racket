(include "tak.scm")
(include "fib.scm")
(include "mbrot.scm")
(include "pnpoly.scm")
(include "deriv.scm")

(define benchmarks 
    (list
        (list tak-main '(1 32 16 8 9))
        (list fib-main '(1 10 55))
        (list mbrot-main '(1 75 5))
        (list pnpoly-main (list 1
                            '#(0. 1. 1. 0. 0. 1. -.5 -1. -1. -2. -2.5 -2. -1.5 -.5 1. 1. 0. -.5 -1. -.5)
                            '#(0. 0. 1. 1. 2. 3. 2. 3. 0. -.5 -1.  -1.5 -2. -2. -1.5 -1. -.5 -1. -1. -.5) 6))
        (list deriv-main (list 1 
                            '(+ (* 3 x x) (* a x x) (* b x) 5)
                            '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
                                (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
                                (* (* b x) (+ (/ 0 b) (/ 1 x))) 0)))))

(scheme-start
    (lambda ()
        (for-each 
            (lambda (b)
                 (let ([bench-proc (car b)]
                       [args (cadr b)])
                     (apply bench-proc args)
                     (newline))) 
            benchmarks)))
