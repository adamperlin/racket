(include "tak.scm")
(include "fib.scm")
(include "mbrot.scm")
(include "pnpoly.scm")

(define benchmarks 
    (list
        (list tak-main '(1 32 16 8 9))
        (list fib-main '(1 10 55))
        (list mbrot-main '(1 75 5))
        (list pnpoly-main (list 1
                            '#(0. 1. 1. 0. 0. 1. -.5 -1. -1. -2. -2.5 -2. -1.5 -.5 1. 1. 0. -.5 -1. -.5)
                            '#(0. 0. 1. 1. 2. 3. 2. 3. 0. -.5 -1.  -1.5 -2. -2. -1.5 -1. -.5 -1. -1. -.5) 6))))

(scheme-start
    (lambda ()
        (for-each 
            (lambda (b)
                 (let ([bench-proc (car b)]
                      [args (cadr b)])
                     (apply bench-proc args)
                     (newline))) 
            benchmarks)))
