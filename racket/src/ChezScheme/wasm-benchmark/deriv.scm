(import (rnrs base)
        (rnrs io simple))

(include "benchmark-common.scm")

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(define (deriv a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (car a) '+)
         (cons '+
               (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '-
               (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
                a
                (cons '+
                      (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else
         (error #f "No derivation method available"))))

(define (main . args)
  (let* ((count (car args))
         (input1 (cadr args))
         (output (caddr args))
         (s (number->string count))
         (name "deriv"))
    (run-r6rs-benchmark
     (string-append name ":" s)
     count
     (lambda () (deriv (hide count input1)))
     (lambda (result) (equal? result output)))))

(main 10000000 
    '(+ (* 3 x x) (* a x x) (* b x) 5)
    '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
        (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
        (* (* b x) (+ (/ 0 b) (/ 1 x))) 0))
