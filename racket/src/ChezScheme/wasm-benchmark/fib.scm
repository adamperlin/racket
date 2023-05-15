;;; FIB -- A classic benchmark, computes fib(n) inefficiently.

(import (rnrs base) (rnrs io simple))
(include "benchmark-common.scm")

;;; The following code is appended to all benchmarks.

;;; Given an integer and an object, returns the object
;;; without making it too easy for compilers to tell
;;; the object will be returned.

(define (hide r x)
  (call-with-values
   (lambda ()
     (values (vector values (lambda (x) x))
             (if (< r 100) 0 1)))
   (lambda (v i)
     ((vector-ref v i) x))))

;;; Given the name of a benchmark,
;;; the number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; and a unary predicate that is true of the
;;; correct results the thunk may return,
;;; runs the benchmark for the number of specified iterations.
;;;
;;; Implementation-specific versions of this procedure may
;;; provide timings for the benchmark proper (without startup
;;; and compile time).

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
  
(define (main . args)
  (let* ((count (car args))
         (input (cadr args))
         (output (caddr args))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fib"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fib (hide count input)))
     (lambda (result) (= result output)))))

(main 3 40 102334155)