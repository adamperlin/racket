(define (hide r x)
  (call-with-values
   (lambda ()
     (values (vector values (lambda (x) x))
             (if (< r 100) 0 1)))
   (lambda (v i)
     ((vector-ref v i) x))))

(define (fractional-time d)
        (fl+ (fixnum->flonum (time-second d)) 
            (fl/ (fixnum->flonum (time-nanosecond d)) (fixnum->flonum 1000000000))))

(define-syntax (collect-real-time stx)
    (syntax-case stx ()
        [(_ call-form)
            #'(let ([s1 (statistics)])
                (let ([before (statistics)])
                    (let ([result call-form])
                        (let ([after (statistics)])
                            (let ([diff (sstats-difference (sstats-difference after before)
                                                                (sstats-difference before s1))])
                                (values result (fractional-time (sstats-real diff))))))))]))

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

(define (run-r6rs-benchmark name count thunk ok?)
  (display "Running ")
  (display name)
  (newline)
  (let-values ([(res times) 
    (let loop ([i 0]
             [result (if #f #f)]
             [times '()])
        (cond [(< i count)
                (let-values ([(result t) (collect-real-time (thunk))])
                    (loop (+ i 1) result (cons t times)))]
            [(ok? result)
            (values result times)]
            [else
            (display "ERROR: returned incorrect result: ")
            (write result)
            (newline)
            (values result times)]))])
        (let ([avg-cpu-time (if (null? times) 
                                0.
                                (fl/ (fold-left fl+ 0. times) (fixnum->flonum (length times))))])
            (printf "Average time per iteration (~a iterations): ~a" count avg-cpu-time))))

