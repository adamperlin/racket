#lang racket

(require ffi/unsafe/vm disassemble)

(define args (current-command-line-arguments))
(unless (> (vector-length args) 1)
    (error 'disassemble-obj "usage: disassemble.rkt <chez-scheme-obj> <function-name>"))

(define path (vector-ref args 0))
(define fn-name (vector-ref args 1))

(unless (file-exists? path)
    (error 'disassemble-obj "file ~a does not exist" path))

(define fn 
    (vm-eval `(begin
                (load ,path) 
                ,(string->symbol fn-name))))

(disassemble fn)