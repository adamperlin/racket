(define (flatten-l2 lst)
      (cond
        [(equal? lst '()) '()]
        [(and (pair? (car lst)) (pair? (caar lst))) (append (flatten-l2 (car lst)) (flatten-l2 (cdr lst)))]
        [else (cons (car lst) (flatten-l2 (cdr lst)))]))

  (define next-local
    (lambda ()
      (define do-iter
        (lambda ()
          (call/cc control-state)))
      (define control-state
        (lambda (return)
          (let loop ([i 0])
            (set! return (call/cc
              (lambda (resume)
                (set! control-state resume)
                (return i))))
            (loop (+ 1 i)))))
      do-iter))

  (define (shift-for-size size)
    (flonum->fixnum (log size 2)))

  (define local-gen (next-local))

  (define (generate-regs-lhs dest ms reg-size tmp)
      `((i32.const ,dest)
        (i32.const ,(shift-for-size reg-size)) ;; 2^3 = sizeof(pb register)
        (i32.shl)
        (local.get ,ms)
        (i32.add)
        (local.set ,tmp)))
    
  (define pb-num-regs 16)
    
  (define (generate-fpregs-lhs fp-dest ms reg-size out)
    `(
      ; fpregs are stored after regs in machine state struct,
      ; so first add sizeof regs array
      (i32.const ,(* pb-num-regs reg-size))

    ; now, calculate the offset into the fpregs array
    ; and add to base offset
      (i32.const ,fp-dest)
      (i32.const ,(shift-for-size reg-size))
      (i32.shl)
      (i32.add)

      ; add result to machine state pointer which will be our destination address
      (local.get ,ms)
      (i32.add)

      (local.set ,out)))

  ; simple wrapper to load from an address (with no offset) and store in
  ; a desired local variable
  (define (load-and-set ptr out)
    `((local.get ,ptr)
      (i32.const 0)
      (i32.load)
      (local.set ,out)))

  (define-syntax (wasm-emit stx)
    (syntax-case stx (scope)
      [(_ (scope n) forms ...)
        #'(uniquify-free-vars
            (flatten-l2
              `(forms ...))
            local-gen n)]
      [(_ forms ...)
        #'(uniquify-free-vars
            (flatten-l2
              `(forms ...))
            local-gen #f)]))

  (define (emit-pb-mov16-pb-zero-bits-pb-shift dest imm-unsigned shift ms)
    (wasm-emit
      ,(generate-regs-lhs dest ms 8 '$_0)
      (local.get $_0)
        ,(case shift
          [(0) `(i32.const ,imm-unsigned)]
          [(1) `((i32.const ,imm-unsigned) (i32.const 16) (i32.shl))]
          [(2) `((i32.const ,imm-unsigned) (i32.const 32) (i32.shl))]
          [(3) `((i32.const ,imm-unsigned) '(i32.const 48) '(i32.shl))])
          (i64.extend_i32_u)
          (i64.store)))

  (define (emit-pb-mov16-pb-keep-bits-pb-shift dest imm-unsigned shift ms)
    (wasm-emit
      ,(generate-regs-lhs dest ms 8 '$_0)
      ,(case shift
          [(0) `(i32.const ,imm-unsigned)]
          [(1) `((i32.const ,imm-unsigned) (i32.const 16) (i32.shl))]
          [(2) `((i32.const ,imm-unsigned) (i32.const 32) (i32.shl))]
          [(3) `((i32.const ,imm-unsigned) (i32.const 48) (i32.shl))])
      (i64.extend_i32_u)
      (local.set $_1)

      ,(load-and-set '$_0 '$_2)

      (local.get $_0)

      (local.get $_1)
      (local.get $_2)
      (i64.or)

      (i64.store)))
    
    (define (emit-pb-mov-pb-i-i dest reg ms)
      (wasm-emit
        (local $_0 i32)
        (local $_1 i32)
        (local $_2 i64)
        ,(generate-regs-lhs dest ms 8 '$_0)
        ,(generate-regs-lhs reg ms 8 '$_1) 
        ,(load-and-set '$_1 '$_2)

        (local.get $_0)
        (local.get $_2)
        (i64.store)))
    
    (define (do-pb-mov-pb-d-d fp-dest fp-reg ms)
      (wasm-emit
        (local $_0 i32)
        (local $_1 i32)
        (local $_2 f64)
        ,(generate-fpregs-lhs fp-dest ms '$_0) 
        ,(generate-fpregs-lhs fp-reg ms '$_1)
        ,(load-and-set '$_1 '$_2)
        (local.get $_0)
        (local.get $_2)
        (i64.store)))
    
    (define (do-pb-mov-i-d fp-dest reg ms)
      (wasm-emit
        (local $_0 i32)
        (local $_1 i32)
        (local $_2 i64)
        ,(generate-fpregs-lhs fp-dest ms 8'$_0) 
        ,(generate-regs-lhs reg ms 8 '$_1)
        ,(load-and-set '$_1 '$_2)

        (local.get $_0)

        (local.get $_2)
        (f64.convert_i32_s)

        (f64.store)))
    
    (define (do-pb-mov-d-i reg-dest fp-reg ms)
      (wasm-emit
        (local $_0 i32)
        (local $_1 i32)
        (local $_2 f64)
        ,(generate-regs-lhs reg-dest ms 8 '$_0)
        ,(generate-fpregs-lhs fp-reg ms 8 '$_1)
        ,(load-and-set '$_1 '$_2)

        (local.get $_0)
        (local.get $_2)
        (i64.trunc_f64_s)

        (i64.store)))
    
    (define (do-pb-mov-i-bits-d-bits fp-dest reg ms)
      (wasm-emit
        (local $_0 i32)
        (local $_1 i32)
        (local $_2 i64)
        ,(generate-fpregs-lhs fp-dest ms 8 '$_0)
        ,(generate-regs-lhs reg ms 8 '$_1)
        ,(load-and-set '$_1 '$_2)

        (local.get $_0)

        (local.get $_2)
        (f64.reinterpret_i64)
        (f64.store)))

  (define (do-pb-mov-d-bits-i-bits reg-dest fp-reg ms)
    (wasm-emit
      (local $_0 i32)
      (local $_1 i32)
      (local $_2 f64)
      ,(generate-regs-lhs reg-dest ms 8 '$_0)
      ,(generate-fpregs-lhs fp-reg ms 8 '$_1)
      ,(load-and-set '$_1 '$_2)

      (local.get $_0)

      (local.get $_2)
      (i64.reinterpret_f64)

      (i64.store)))

  ; note: mov hi bits to low bits instructions skipped for now

  (define (do-pb-mov-s-d fpregs-dest fpreg ms endian)
    (wasm-emit
      (local $_0 i32)
      (local $_1 i32)
      (local $_2 f64)
      ,(generate-regs-lhs fpregs-dest ms 8 '$_0)
      ,(generate-regs-lhs fpreg ms 8 '$_1)

      (local.get $_1)
      (f32.load offset=,(if (equal? endian 'big) 4 0))
      (f64.promote_f32)

      (local.set $_2)

      (i32.load $_0)
      (local.get $_2)

      (f64.store)))

  (define (load-from-reg reg ms dst)
    ; introduce a new scope prefix '%'
    ; to avoid conflicts with the dst passed
    ; in. We only want to assign unique names to variables
    ; introduced within this "scope"
    (wasm-emit (scope '%)
      (local %0 i32)
      (local %1 i32)
      ,(generate-regs-lhs reg ms 8 '%0) 
      ,(load-and-set '%0 '%1)
      (local.get %1)
      (local.set ,dst)))

  (define (load-from-fpreg fpreg ms dst)
    ; introduce a new scope prefix '%'
    ; to avoid conflicts with the dst passed
    ; in. We only want to assign unique names to variables
    ; introduced within this "scope"
    (wasm-emit (scope '%)
      (local %0 i32)
      (local %1 i32)
      ,(generate-fpregs-lhs fpreg ms 8 '%0) 
      ,(load-and-set '%0 '%1)
      (local.get %1)
      (local.set ,dst)))

  (define (do-pb-mov-d-s fpreg-dest fpreg ms endian)
    (wasm-emit
      (local $_0 i32)
      (local $_1 f64)
      (local $_2 i32)
      ,(generate-regs-lhs fpreg ms 8 '$_0)
      ,(load-and-set '$_0 '$_1)
      ,(generate-regs-lhs fpreg-dest ms 8 '$_2)

      (local.get $_2)
      ,(if (equal? endian 'big)
        `((i32.const 4) (i32.add))
        `())

      (f64.get $_1)
      (f32.demote_f64)

      (f32.store)))

  (define (do-pb-mov-d-s-d fpregs-dest fpreg ms)
    (wasm-emit
      (local $_0 f64)
      (local $_1 i32)
      (load-from-fpreg fpreg ms '$_0)
      ,(generate-fpregs-lhs fpreg ms 8 '$_1)

      (local.get $_1)

      (local.get $_0)
      (f32.trunc_f64_s)
      (f64.promote_f32)

      (f64.store)))

  (define (do-pb-binop-no-signal op op1 op2)
    (wasm-emit (scope '%) ; use a different scope indicator so we can use passed in op1 and op2
                          ; and we don't rewrite them in this nested context
      (local.get ,op1) 
      (local.get ,op2)
      ,(case op
        [(add) '(i64.add)]
        [(sub) '(i64.sub)]
        [(mul) '(i64.mul)]
        [(div) '(i64.div_s)]
        [(and) '(i64.and)]
        [(ior) '(i64.or)]
        [(xor) '(i64.xor)]
        
        ; NOTE: clarify presence of SHIFT_MASK in pb.h
        [(lsl) '(i64.shl)]
        [(lsr) '(i64.shr_u)]
        [(asr) '(i64.shr_s)]
        ; TODO: lslo implementations
      )))

  (define (do-pb-bin-op-pb-no-signal-pb-register dest reg1 reg2 ms op)
    (wasm-emit
      ,(load-from-reg reg1 ms '$_0)
      ,(load-from-reg reg2 ms '$_1)
      ,(generate-regs-lhs dest ms 8 '$_2)
      ; load address to store to
      (local.get $_2)
      ; do op
      ,(do-pb-binop-no-signal op '$_0 '$_1)
      ; store to perform assignment
      (i64.store)))

  (define (do-pb-bin-op-pb-no-signal-pb-immediate dest reg imm ms op)
    (wasm-emit
      ,(load-from-reg reg ms '$_0)
      (i32.const imm)
      (i64.extend_i32_u)
      (local.set '$_1)
      ,(generate-regs-lhs dest ms 8 '$_2)

      ; load address to store to
      (local.get $_2)
      ; do op
      ,(do-pb-binop-no-signal op '$_0 '$_1)
      ; store to perform assignment
      (i64.store)))

  ;; TODO: overflow-enabled operations. Need to determine how to call __builtin_add_overflow etc.

  (define (do-pb-subzp-pb-register dest reg1 reg2 ms zp flag)
    (wasm-emit
      (local $_0 i64)
      (local $_1 i64)
      (local $_2 i32)
      (local $_3 i64)

      ,(load-from-reg ms '$_0) 
      ,(load-from-reg ms '$_1)
      ,(generate-regs-lhs dest ms 8 '$_2)

      ; perform subz
      (local.get $_0)
      (local.get $_1)
      (i64.sub)
      (local.set $_3)

      ; load address to assign to
      (local.get $_2)

      ; load result
      (local.get $_3)

      ; store to perform assignment
      (i64.store)

      ; perform eqz
      (local.get $_3)
      ,(case zp
        [(z) '(i64.eqz)]
        [(p) '((i64.const 0) (i64.gt_s))])

      ; assign to flag
      (local.set ,flag)))

  (define (do-pb-subzp-pb-immediate dest reg1 imm ms zp flag)
    (wasm-emit
      (local $_0 i64)
      (local $_1 i64)
      (local $_2 i32)
      (local $_3 i64)

      ,(load-from-reg ms '$_0) 
      (i32.const imm)
      (i64.extend_i32_u)
      (local.set $_1)
      ,(generate-regs-lhs dest ms 8 '$_2)

      ; load address to store to
      (local.get $_2)

      ; perform sub
      (local.get $_0)
      (local.get $_1)
      (i64.sub)
      (local.set $_3)

      ; store to perform assignment
      (i64.store)

      (local.get $_3)
      ,(case zp
        [(z) '(i64.eqz)]
        [(p) '((i64.const 0) (i64.gt_s))])

      (local.set ,flag)))

  (define (do-pb-cmp-op-no-signal op op1 op2)
    (wasm-emit (scope '%)
      (local.get ,op1)
      (local.get ,op2)
      ,(case op
        [(eq) '(i64.eq)]
        [(lt) '(i64.lt_s)]
        [(gt) '(i64.gt_s)]
        [(le) '(i64.le_s)]
        [(ge) '(i64.ge_s)]
        [(ab) '(i64.gt_u)]
        [(bl) '(i64.lt_u)]
        [(cs) '(
          (i64.and)
          (i64.const 0)
          (i64.ne))]
        [(cc) '(
          (i64.and)
          (i64.const 0)
          (i64.eq))])))

  (define (emit-pb-cmp-op-pb-register reg1 reg2 ms cmp-op flag)
    (wasm-emit
      ,(load-from-reg reg1 ms '$_0)
      ,(load-from-reg reg2 ms '$_1)
      ,(do-pb-cmp-op-no-signal cmp-op '$_0 '$_1)

      (local.set ,flag)))
    
  (define (emit-pb-cmp-op-pb-immediate reg imm ms cmp-op flag)
    (wasm-emit
      ,(load-from-reg reg ms '$_0)

      (i32.const ,imm)
      (i64.extend_i32_u)
      (local.set $_1)

      ,(do-pb-cmp-op-no-signal cmp-op '$_0 '$_1)

      (local.set ,flag)))
  
  (define (emit-pb-literal dest ms word-size literal-offset)
    (define val-type 
      (case word-size 
        [(4) 'i32]
        [(8) 'i64]
        [else ($oops 'wasm-emit "unsupported word size ~a" word-size)]))
    (define load-op (string->symbol (format "~a.load" val-type)))
    (define store-op (string->symbol (format "~a.store" val-type)))
    (wasm-emit
      (local $_0 i32)
      (local $_1 i64)
      ,(generate-regs-lhs dest ms word-size '$_0)

      (local.get $ip)
      (i32.const ,literal-offset)
      (i32.add)

      (,load-op)

      (local.set $_1)

      (local.get $_0)
      (local.get $_1)
      (,store-op)))
  
  (define (pb-load-type->wasm-load-type src-type dest-type)
    (case src-type
      [(int8) 'load8_s]
      [(uint8) 'load8_u]
      [(int16) 'load16_s]
      [(uint16) 'load16_u]
      [(int32) (if (equal? dest-type 'i32) 'load_s 'load32_s)]
      [(uint32) (if (equal? dest-type 'i32) 'load_u 'load32_u)]
      [(int64) 
        (begin
          (unless (equal? dest-type 'i64)
            ($oops 'wasm-emit "incompatible load type ~a for ~a" src-type dest-type))
          'load)]
      [(single) 'load] 
      [(double) 
        (begin
          (unless (equal? dest-type 'f64)
            ($oops 'wasm-emit "incompatible load type ~a for ~a" src-type dest-type))
          'load)]))

   (define (word-size->val-type word-size)
    (case word-size
      [(4) 'i32] 
      [(8) 'i64]
      [else ($oops 'wasm-emit "unsupported word size ~a" word-size)]))

   (define (word-size->dest-type word-size fp)
    (if fp
      (case word-size
        [(4) 'f32] 
        [(8) 'f64]) 
      (case word-size
        [(4) 'i32] 
        [(8) 'i64])))
  
  (define (is-fp? src-type)
    (or (equal? src-type 'single) (equal? src-type 'double)))
    
  (define (emit-pb-ld-pb-immediate dest base imm ms src-type word-size)
    (define dest-type (word-size->dest-type word-size (is-fp? src-type)))
    (define load-type (pb-load-type->wasm-load-type src-type dest-type))
    (define load-op
      (string->symbol (format "~a.~a"  dest-type load-type)))
    (define store-op (string->symbol (format "~a.store" dest-type)))

    (wasm-emit
      (local $_0 i32)
      (local $_1 i32)
      ,(if (or (equal? dest-type 'single) (equal? dest-type 'double))
         (generate-fpregs-lhs dest ms word-size '$_0)
         (generate-regs-lhs dest ms word-size '$_0))
      ,(load-from-reg base ms '$_1)

      (local.get $_1)
      (i32.const ,imm)
      (,load-op)
      (local.set $_2)

      (local.get $_0)
      (local.get $_2)

      (,store-op)))
  
  (define (emit-pb-b*-pb-immediate base target-offset ms)
    (wasm-emit
      (local $_0 i32)
      ,(load-from-reg base ms '$_0)
      (i32.const ,target-offset)
      (i32.add)
      (return)))
  
  (define (emit-pb-b-pb-immediate target-offset next-instr ms test)
    (wasm-emit
      ,(if (null? test) '(i32.const 1) test)
      (if 
          (block
            (local.get $ip)
            (i32.const ,target-offset)
            (i32.add)
            (return))
          (return (i32.add 
                    (local.get $ip) 
                    (i32.const ,next-instr))))))
  
  (define (emit-pb-b-pb-register reg next-instr ms test)
    (wasm-emit
      ,(if (null? test) '(i32.const 1) test)
      (if 
        (block 
          (local $_0 i32) 
          ,(load-from-reg reg ms '$_0)
          (return))
        (return (i32.add (local.get $ip) 
                        (i32.const next-instr))))))
  
  (define (format-with-newlines wasm-sexp)
    (fold-left 
      string-append
      ""
      (map (lambda (x) (format "\t~a\n" x)) wasm-sexp)))
  
  ;; TODO: fp bin ops and fp cmp ops

  ; (define (first-index-of s c)
  ;   (let loop ([i 0] [str-lst (string->list s)])
  ;     (cond
  ;       [(empty? s) #f]
  ;       [(equal? (car str-lst) c) i]
  ;       [else (loop (+ 1 i) (cdr str-lst))])))

  (define (is-temp-var? sym)
    (let ([name (symbol->string sym)])
      (and (>= (string-length name) 2)
        (equal? (substring name 0 2) "$_"))))

  (define (has-prefix? sym prefix)
    (let ([name (symbol->string sym)]
          [pref-len (string-length prefix)])
      (and (>= (string-length name) pref-len)
        (equal? (substring name 0 pref-len) prefix))))

  (define (uniquify-free-vars wasm-tree local-gen only-scope)
    (define assigned (make-hashtable symbol-hash equal?))
    (define default-prefix "$_")
    (define prefix (if (symbol? only-scope) 
                          (symbol->string only-scope)
                          default-prefix))
    (let traverse ([node wasm-tree])
      (cond
        [(list? node) (map traverse node)]
        [(and (symbol? node) (has-prefix? node prefix))
          (if (symbol-hashtable-contains? assigned node)
            (symbol-hashtable-ref assigned node 0)
            (let ([new-name (format "$~a" (local-gen))])
                (symbol-hashtable-set! assigned node new-name)
                  new-name))]
                  
        [else node])))