;; This script helps convert boot files to pbchunk format, staging
;; everything in a new boot-file directory

(define who 'wasm-bootpbchunk)

(define args (command-line-arguments))
(when (null? args)
    (error who "missing arguments"))

(define-record-type flags
  (fields
    (mutable petite?)
    (mutable dest)
    (mutable arch)
    (mutable extra-boot)
    (mutable only-funcs)
    (mutable exclude-funcs)))
  
(define (parse-args args arg-state)
  (cond 
    [(null? args) arg-state]
    [(equal? (car args) "--petite")
      (flags-petite?-set! arg-state #t)
      (parse-args (cdr args) arg-state)]

    [(equal? (car args) "--dest")
      (flags-dest-set! arg-state (cadr args))
      (parse-args (cddr args) arg-state)]
    
    [(equal? (car args) "--arch" )
      (flags-arch-set! arg-state (cadr args))
      (parse-args (cddr args) arg-state)]

    [(equal? (car args) "--extra-boot")
      (flags-extra-boot-set! arg-state (cadr args))
      (parse-args (cddr args) arg-state)]

    [(equal? (car args) "--exclude-funcs")
     (flags-exclude-funcs-set! arg-state (cdr args))]
    
    [(equal? (car args) "--only-funcs")
      (flags-only-funcs-set! arg-state (cdr args))]))

(define arg-state (make-flags #f "pbchunk-out" "pb64l" "" '() '()))
(parse-args args arg-state)

(define petite? (flags-petite? arg-state))
(define dest (flags-dest arg-state))
(define arch (flags-arch arg-state))
(define extra-boot (flags-extra-boot arg-state))
(define only-funcs (flags-only-funcs arg-state))
(define exclude-funcs (flags-exclude-funcs arg-state))

(define petite-boot (format "boot/~a/petite.boot" arch))

(define boots (if petite?
                (list petite-boot extra-boot)
                (list extra-boot)))


(printf "boots:  ~a\n" boots)
(printf "dest-dir: ~a\n" dest)
(printf "arch: ~a\n" arch)
(printf "funcs: ~a" only-funcs)

(when (not (file-exists? dest))
  (mkdir dest))

; (when (null? args)
;   (error who "missing srcdir"))
; (when (null? (cdr args))
;   (error who "missing target"))

; (define srcdir (car args))
; (define destdir (cadr args))
; (define target (caddr args))
; (define args (cdddr args))

; (define scheme? (and (pair? args)
;                      (equal? "--scheme" (car args))))
; (define petite? (and (pair? args)
;                      (equal? "--petite" (car args))))
; (define only? (and (pair? args)
;                    (equal? "--only" (car args))))
; (define more-boots (if (or petite? scheme? only?)
;                        (cdr args)
;                        args))
; (when (pair? more-boots)
;   (let ([s (car more-boots)])
;     (when (and (positive? (string-length s))
;                (eqv? #\- (string-ref s 0)))
;       (error who "unrecognized flag ~s" s))))

; (for-each (lambda (more-boot)
;             (let loop ([ml (reverse (string->list more-boot))]
;                        [bl (reverse (string->list ".boot"))])
;               (unless (null? bl)
;                 (when (or (null? ml)
;                           (not (eqv? (car ml) (car bl))))
;                   (error who "~s does not end with \".boot\"" more-boot))
;                 (loop (cdr ml) (cdr bl)))))
;           more-boots)

; (define boots (cond
;                 [only? '()]
;                 [petite? (list petite-boot)]
;                 [else (list petite-boot scheme-boot)]))

; (define src-target
;   (list->string (let loop ([l (string->list target)])
;                   (cond
;                     [(null? l)
;                      (error 'bootchunk "no `-` in target ~s" target)]
;                     [(eqv? #\- (car l)) '()]
;                     [else (cons (car l) (loop (cdr l)))]))))

; (define src (let ([p (string-append destdir "/boot/" src-target)])
;               (if (file-directory? p)
;                   p
;                   (string-append srcdir "/" src-target))))

; (unless (file-directory? src)
;   (error who "cannot find base bootfiles for ~s" src-target))

; (define xpatch (and (not (equal? src-target (symbol->string (machine-type))))
;                     (format "~a/xc-~a/s/xpatch" destdir src-target)))
; (unless (or (not xpatch)
;             (file-exists? xpatch))
;   (error who "cannot find cross patch file ~s" xpatch))

; (for-each (lambda (f)
;             (unless (file-exists? f)
;               (error who "file not found: ~s" f)))
;           '(srcfile))

(when (not (file-exists? dest))
  (mkdir dest))
  
(for-each (lambda (f)
            (delete-file (string-append dest "/" f)))
          (directory-list dest))

(define (copy-file f)
  (let ([i (open-file-input-port (string-append src "/" f))]
        [o (open-file-output-port (string-append dest "/" f) (file-options no-fail))]
        [buf (make-bytevector 4096)])
    (let loop ()
      (let ([n (get-bytevector-n! i buf 0 (bytevector-length buf))])
        (unless (eof-object? n)
          (put-bytevector o buf 0 n)
          (loop))))
    (close-input-port i)
    (close-output-port o)))

(define dest-boots (append (map (lambda (f) (string-append dest "/" (path-last f)))
                                boots)))
(define src-boots boots)

;   (fprintf o "extraCSources=~a~a\n"
;            (apply string-append
;                   (apply append
;                          (map (lambda (src-boot)
;                                 (let ([name (extract-boot-name src-boot)])
;                                   (many (string-append "pbchunk_" name "~a.c "))))
;                               src-boots)))
;            "pbchunk_register.c")
;   (close-port o))

(define (extract-boot-name f)
  (list->string
   (let loop ([l (string->list (path-last f))])
     (cond
       [(null? l) '()]
       [(eqv? #\. (car l)) '()]
       [else (cons (car l) (loop (cdr l)))]))))

(define (format-with-index fmt)
  (let loop ([i 0])
    (if (eqv? i 10)
        '()
        (cons (format fmt i) (loop (add1 i))))))

(define (do-wasm-pbchunk src-boots dest-boots)
  (let loop ([src-boots src-boots]
             [dest-boots dest-boots]
             [index 0]
             [wat-files '()])
    (cond
      [(null? src-boots) wat-files]
      [else
        (printf "Convert ~s\n" (car src-boots))
        (let ([name (extract-boot-name (car src-boots))])
          (let ([new-wat-file (format (string-append dest "/" "pbchunk_" name "~a.wat") index)])
            (let ([index (wasm-pbchunk-convert-file 
                                              (car src-boots)
                                              (car dest-boots)
                                              new-wat-file
                                              index
                                              only-funcs
                                              exclude-funcs)])
              (loop (cdr src-boots)
                    (cdr dest-boots)
                    index
                    (append wat-files (list new-wat-file))))))])))
  
(define new-wat-files (do-wasm-pbchunk src-boots dest-boots))
(let ([o (open-file-output-port (format "boot/~a/Mf-config" arch)
                                (file-options no-fail)
                                (buffer-mode block)
                                (current-transcoder))])
  (fprintf o "extraWasmSources=~a\n"
           (apply string-append
                  (map (lambda (path) (format "\"../../~a\"" path))
                      new-wat-files))))