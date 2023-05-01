(define (add3 x y)
  (+ x y 3))

(define (fact n)
  (if (<= n 0) 1
    (* n (fact (- n 1)))))

(define (bitwise-ops fx1 fx2)
  (fxand 
    (fxnot (fxior fx1 fx2))
    (fxior (fxsrl fx1 3) (fxsll fx1 3))))

(define (hypot fl1 fl2) 
  (fxsqrt (fx+ (fx* fl1 fl1) (fx* fl2 fl2))))

;; splits a list in two
(define (split-lst lst)
  (define half-len (quotient (length lst) 2))
  (let ([res
           (fold-right (lambda (e acc)
            (let ([l1 (car acc)]
                  [l2 (car (cdr acc))]
                  [i (car (cdr (cdr acc)))])
                (if (< i half-len)
                    (list (cons e l1) l2 (+ 1 i))
                    (list l1 (cons e l2) (+ 1 i)))))
         (list '() '() 0)
         lst)])
    (list (car (cdr res)) (car res))))

(define (my-merge l1 l2)
        (cond
            [(null? l1) l2]
            [(null? l2) l1]
            [(< (car l1) (car l2)) (cons (car l1) (my-merge (cdr l1) l2))]
            [else (cons (car l2) (my-merge l1 (cdr l2)))]))

(define (simple-cons x)
  (cons x '()))

(define (merge-sort lst)
  (cond 
    [(null? lst) '()]
    [(equal? (length lst) 1) lst]
    [else 
      (let ([halves (split-lst lst)])
        (my-merge (merge-sort (car halves))
                  (merge-sort (cadr halves))))]))

; test overflow operations
; test continuation capture operations
; test load/stores of values with differing fixed sizes
; more floating point tests?

(scheme-start 
   (lambda ()
      (printf "~a\n" (my-merge '(1 4 5 8) '(2 3 7)))
      (printf "~a\n" (merge-sort '(10 -1 2 0 7 5 9)))
      (printf "~a\n" (split-lst '(1 2 3 4 5 6)))
      (assert (equal? (simple-cons 1) '(1)))
      (assert (equal? (merge-sort '(10 -1 2 0 7 5 9)) '(-1 0 2 5 7 9 10)))
      (assert (= (add3 10 11) 24))
      (assert (= (fact 10) 3628800))))
