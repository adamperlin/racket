; (define (fib-iter-cc n)

;     (define (loop i n a b)
;         (define (continue-loop (loop (call/cc (lambda (k) (k k))))))
;         (if (= i n) b
;             (continue-loop (+ 1 i) n b (+ a b)))
;     ))


(define (fib-cc n)
    (let-values 
        ([(a b i next) (call/cc (lambda (k) (k 0 1 1 k)))])
        (if (= i n) b
            (next b (+ a b) (+ i 1) next))))

(display (fib-cc 40))