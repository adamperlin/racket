(define bench-boot-file-name "bench.boot")
(define source-files (list "main.scm"))
(define target-files (list "main.so"))

(map compile-file source-files)
(apply make-boot-file 
    (append
        (list 
            bench-boot-file-name 
            (list "petite"))
        target-files))


