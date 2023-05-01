(define test-boot-file-name "wasm-pbchunk-tests.boot")
(define source-files (list "wasm-pbchunk-tests.ss"))
(define target-files (list "wasm-pbchunk-tests.so"))

(map compile-file source-files)
(apply make-boot-file 
    (append
        (list 
            test-boot-file-name 
            (list "petite"))
        target-files))


