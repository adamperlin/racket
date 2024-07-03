;; This file contains provides functions which translate PB instructions into Wasm
;; It is currently only used by wasm-pbchunk.ss, but is meant to be able to be
;; included anywhere where instruction-level translation of PB->Wasm is needed.

;; SOME CONVENTIONS
;; The wasm-emit macro is meant to be used for generating most wasm. 
;; Hopefully, this macro can be continually expanded if more sophisticated
;; code generation is implemented, to abstract away some of the boilerplate
;; involved in generating wasm. It allows for generating temporary locals on the fly. The locals are hoisted 
;; later by hoist-locals, since wasm locals must be declared at the beginning of 
;; a function

;; Most of the codegen functions take an `ms` parameter,
;; which holds the name of machine state parameter which is passed
;; to a generated wasm chunk. Generally, this will be "$ms". They 
;; may also take `dest` and `reg` parameters, which are nearly always the INDEX
;; of a particular register. 

;; We need to be careful about sign extension when converting from an immediate value 
;; that is embedded in a PB instruction, and a static offset in generated Wasm. 
;; Almost any time an immediate OFFSET is used for an address computation,
;; the immediate offset should be sign-extended. Most of the time when 
;; there is an `imm` present as a parameter, it is assumed that the `imm`
;; is a scheme integer, which may need to be sign extended in generated wasm
;; (for example, see emit-pb-bin-op-pb-no-signal-pb-immediate).

;; `8` is hardcoded as register size in most places where these functions are called.
;; Thus, the codegen has been tested assuming a 64-bit PB register size,
;; and is untested on 32-bit PB. Some adjustments will likely need to be made
;; to support 32-bit PB. 


;; flattens away a level of nesting
;; i.e., '(((i32.const 1) (i32.const 2) (i32.add)) (i32.sub)) ->
;; '((i32.const 1) (i32.const 2) (i32.add) (i32.sub))
(define (flatten-l2 lst)
      (cond
        [(equal? lst '()) '()]
        [(and (pair? (car lst)) (pair? (caar lst))) (append (flatten-l2 (car lst)) (flatten-l2 (cdr lst)))]
        [else (cons (car lst) (flatten-l2 (cdr lst)))]))

;; helper macro for generating temporaries.

;; generates a scheme local for every name declared in the (locals ([name type]) ...) form,
;; with the *value* of the local being a unique identifier from gensym. This way, the unique
;; identifier can be spliced into generated wasm, or passed to other functions which
;; generate wasm and need a wasm local id.
(define-syntax (wasm-emit stx)
  (syntax-case stx (temp)
    [(_ (locals ([id t] ...)) forms ...)
      #'(let ([id (format "$~a" (syntax->datum (car (generate-temporaries #'(id)))))] ...)
             (flatten-l2 `((local ,id t) ... forms ...)))]
    [(_ forms ...) 
          #'(flatten-l2 `(forms ...))]))

(define (load-for-size n) 
  (case 
    [(4) 'i32.load]
    [(8) 'i64.load]
    [else ($oops 'wasm-emit "invalid word size ~a" n)]))

(define (shift-for-size size)
  (flonum->fixnum (log size 2)))

;; used for assignments to a register. 
;; generates Wasm which computes a pointer to a particular PB register and stores in `tmp`
; params:
;   reg-size: register size in bytes
;   ms: name of the locally declared machine state pointer
;   tmp: name of the local to store into
; returns: generated wasm for commputing an address, storing in tmp
(define (generate-regs-lhs dest ms reg-size tmp)
    `((i32.const ,dest)
      (i32.const ,(shift-for-size reg-size)) ;; 2^3 = sizeof(pb register)
      (i32.shl)
      (local.get ,ms)
      (i32.add)
      (local.set ,tmp)))
    
  (define pb-num-regs 16)
  (define pb-instr-num-bytes 4)

;; same as generate-regs-lhs, but creates a pointer into floating point register
;; array instead
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
; a desired local variable `out`
(define (load-and-set ptr out ty)
  (let ([load-instr `(,(format "~a.load" ty))])
    `((local.get ,ptr)
      ,load-instr
      (local.set ,out))))

; params:
;   dest: index of the register destination fo the mov
;   imm-unsigned: immediate value, must be pre-sign extended! 
;   shift: the number of BYTES to shift the immediate by
; returns: Wasm representing a mov16 instruction, with temporary locals
(define (emit-pb-mov16-pb-zero-bits-pb-shift dest imm-unsigned shift ms)
  (wasm-emit (local ([$0 i32]))
    ,(generate-regs-lhs dest ms 8 $0)
    (local.get ,$0)
      ,(case shift
        [(0) `(i64.const ,imm-unsigned)]
        [(1) `((i64.const ,imm-unsigned) (i64.const 16) (i64.shl))]
        [(2) `((i64.const ,imm-unsigned) (i64.const 32) (i64.shl))]
        [(3) `((i64.const ,imm-unsigned) (i64.const 48) (i64.shl))])
        (i64.store)))

(define (emit-pb-mov16-pb-keep-bits-pb-shift dest imm-unsigned shift ms)
  (wasm-emit
    (local ([$0 i32]
            [$1 i64]
            [$2 i64]))
    ,(generate-regs-lhs dest ms 8 $0)
    ,(case shift
        [(0) `(i64.const ,imm-unsigned)]
        [(1) `((i64.const ,imm-unsigned) (i64.const 16) (i64.shl))]
        [(2) `((i64.const ,imm-unsigned) (i64.const 32) (i64.shl))]
        [(3) `((i64.const ,imm-unsigned) (i64.const 48) (i64.shl))])
    (local.set ,$1)

    ,(load-and-set $0 $2 'i64)

    (local.get ,$0)

    (local.get ,$1)
    (local.get ,$2)
    (i64.or)

    (i64.store)))

; integer -> integer mov
(define (emit-pb-mov-pb-i-i dest reg ms)
  (wasm-emit (local ([$0 i32] [$1 i32] [$2 i64]))
    ,(generate-regs-lhs dest ms 8 $0)
    ,(generate-regs-lhs reg ms 8 $1) 
    ,(load-and-set $1 $2 'i64)

    (local.get ,$0)
    (local.get ,$2)
    (i64.store)))

; double -> double mov
(define (emit-pb-mov-pb-d-d fp-dest fp-reg ms)
  (wasm-emit
    (locals ([$0 i32]
              [$1 i32]
              [$2 f64]))
    ,(generate-fpregs-lhs fp-dest ms 8 $0) 
    ,(generate-fpregs-lhs fp-reg ms 8 $1)
    ,(load-and-set $1 $2 'f64)
    (local.get ,$0)
    (local.get ,$2)
    (f64.store)))

; integer -> double mov, using f64.convert_i64_s
(define (emit-pb-mov-pb-i-d fp-dest reg ms)
  (wasm-emit
    (locals ([$0 i32]
              [$1 i32]
              [$2 i64]))
    ,(generate-fpregs-lhs fp-dest ms 8 $0) 
    ,(generate-regs-lhs reg ms 8 $1)
    ,(load-and-set $1 $2 'i64)

    (local.get ,$0)

    (local.get ,$2)
    (f64.convert_i64_s)

    (f64.store)))

; double -> integer mov
(define (do-pb-mov-d-i reg-dest fp-reg ms)
  (wasm-emit
    (locals ([$0 i32]
              [$1 i32]
              [$2 f64]))
    ,(generate-regs-lhs reg-dest ms 8 $0)
    ,(generate-fpregs-lhs fp-reg ms 8 $1)
    ,(load-and-set $1 $2 'f64)

    (local.get ,$0)
    (local.get ,$2)
    (i64.trunc_f64_s)

    (i64.store)))

; bitcast integer to double
(define (do-pb-mov-i-bits-d-bits fp-dest reg ms)
  (wasm-emit
    (locals ([$0 i32]
              [$1 i32]
              [$2 i64]))
    ,(generate-fpregs-lhs fp-dest ms 8 $0)
    ,(generate-regs-lhs reg ms 8 $1)
    ,(load-and-set $1 $2 'i64)

    (local.get ,$0)

    (local.get ,$2)
    (f64.reinterpret_i64)
    (f64.store)))

; bitcast double to integer
(define (do-pb-mov-d-bits-i-bits reg-dest fp-reg ms)
  (wasm-emit
    (locals ([$0 i32]
              [$1 i32]
              [$2 f64]))
    (local $0 i32)
    (local $1 i32)
    (local $2 f64)
    ,(generate-regs-lhs reg-dest ms 8 $0)
    ,(generate-fpregs-lhs fp-reg ms 8 $1)
    ,(load-and-set $1 $2 'f64)

    (local.get ,$0)

    (local.get ,$2)
    (i64.reinterpret_f64)

    (i64.store)))

; note: mov hi bits to low bits instructions skipped for now (i.e., pb-mov-pb-i-i-d-d)

; upcast single to double
(define (do-pb-mov-s-d fpregs-dest fpreg ms endian)
  (wasm-emit
    (locals ([$0 i32] 
              [$1 i32] 
              [$2 f64]))
    ,(generate-regs-lhs fpregs-dest ms 8 $0)
    ,(generate-regs-lhs fpreg ms 8 $1)

    (local.get ,$1)
    (f32.load offset=,(if (equal? endian 'big) 4 0))
    (f64.promote_f32)

    (local.set ,$2)

    (i32.load ,$0)
    (local.get ,$2)

    (f64.store)))

; generates wasm which loads from the pb register index `reg`, storing
; in a wasm temporary `dst`
(define (load-from-reg reg ms dst)
  (wasm-emit (locals ([$0 i32]
                      [$1 i64]))
    ,(generate-regs-lhs reg ms 8 $0) 
    ,(load-and-set $0 $1 'i64)
    (local.get ,$1)
    (local.set ,dst)))

;; load-from-reg for fp registers
(define (load-from-fpreg fpreg ms dst)
  (wasm-emit (locals ([$0 i32] [$1 f64]))
    ,(generate-fpregs-lhs fpreg ms 8 $0) 
    ,(load-and-set $0 $1 'f64)
    (local.get ,$1)
    (local.set ,dst)))

; truncate double to single
(define (do-pb-mov-d-s fpreg-dest fpreg ms endian)
  (wasm-emit
    (locals ([$0 i32]
              [$1 f64]
              [$2 i32]))
    ,(generate-regs-lhs fpreg ms 8 $0)
    ,(load-and-set $0 $1)
    ,(generate-regs-lhs fpreg-dest ms 8 $2)

    (local.get ,$2)
    ,(if (equal? endian 'big)
      `((i32.const 4) (i32.add))
      `())

    (f64.get ,$1)
    (f32.demote_f64)

    (f32.store)))

; truncate double to single, store as double
(define (do-pb-mov-d-s-d fpregs-dest fpreg ms)
  (wasm-emit
    (locals ([$0 f64]
              [$1 i32]))
    ,(load-from-fpreg fpreg ms $0)
    ,(generate-fpregs-lhs fpreg ms 8 $1)

    (local.get ,$1)

    (local.get ,$0)
    (f32.trunc_f64_s)
    (f64.promote_f32)

    (f64.store)))

; perform binops, where op defines a bin op (see wasm-pbchunk instruction-cases)
;; op1 and op2 must be wasm locals
(define (do-pb-binop-no-signal op op1 op2)
  (wasm-emit
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
      [else (unimplemented op)]
      ; TODO: lslo implementations
    )))

  (define (do-pb-fp-binop-no-signal op op1 op2)
    (wasm-emit
      (local.get ,op1)
      (local.get ,op2)
      ,(case op
        [(add) '(f64.add)]
        [(sub) '(f64.sub)]
        [(mul) '(f64.mul)]
        [(div) '(f64.div)]
        [else (unimplemented op)])))

(define (do-pb-fp-unop op op0)
  (wasm-emit
    (local.get ,op0)
    ,(case op
      [(sqrt) '(f64.sqrt)]
      [else ($oops 'wasm-emit "unsupported pb unary operation ~a" op)])))

(define (emit-pb-fp-unop fp-dest fpreg ms op)
  (wasm-emit
    (locals ([$0 i32] [$1 f64] [$2 f64]))
      ,(generate-fpregs-lhs fp-dest ms 8 $0)
      ,(load-from-fpreg fpreg ms $1)

      (local.get ,$0)
      ,(do-pb-fp-unop op $1)
      (f64.store)))

(define (emit-pb-bin-op-pb-no-signal-pb-register dest reg1 reg2 ms op)
  (wasm-emit
    (locals ([$0 i64] [$1 i64] [$2 i32]))
    ,(load-from-reg reg1 ms $0)
    ,(load-from-reg reg2 ms $1)
    ,(generate-regs-lhs dest ms 8 $2)
    ; load address to store to
    (local.get ,$2)
    ; do op
    ,(do-pb-binop-no-signal op $0 $1)
    ; store to perform assignment
    (i64.store)))

(define (emit-pb-fp-binop dest reg1 reg2 ms op) 
  (wasm-emit
    (locals ([$0 f64] [$1 f64] [$2 i32]))
      ,(load-from-fpreg reg1 ms $0)
      ,(load-from-fpreg reg2 ms $1)
      ,(generate-fpregs-lhs dest ms 8 $2)

      (local.get ,$2)
      ,(do-pb-fp-binop-no-signal op $0 $1)

      (f64.store)))

(define (do-pb-fp-cmp-op cmp-op op1 op2)
  (wasm-emit
    (local.get ,op1)
    (local.get ,op2)

    ,(case cmp-op
      [(eq) '(f64.eq)]
      [(le) '(f64.le)]
      [(lt) '(f64.lt)]
      [else ($oops 'wasm-emit "unsupported fp cmp operation ~a" cmp-op)])))

(define (emit-pb-fp-cmp-op reg1 reg2 ms flag cmp-op)
  (wasm-emit 
    (locals ([$0 f64] [$1 f64]))
    ,(load-from-fpreg reg1 ms $0)
    ,(load-from-fpreg reg2 ms $1)
    ,(do-pb-fp-cmp-op cmp-op $0 $1)
    (local.set ,flag)))

(define (emit-pb-bin-op-pb-no-signal-pb-immediate dest reg imm ms op)
  (wasm-emit
    (locals ([$0 i64]
              [$1 i64]
              [$2 i32]))

    ,(load-from-reg reg ms $0)
    (i32.const ,imm)

    ; sign extension of the immediate is crucial here, as we need the equivalent
    ; signed value to do a binary operation against a register of possibly larger size
    (i64.extend_i32_s)
    (local.set ,$1)
    ,(generate-regs-lhs dest ms 8 $2)

    ; load address to store to
    (local.get ,$2)
    ; do op
    ,(do-pb-binop-no-signal op $0 $1)
    ; store to perform assignment
    (i64.store)))

(define (unimplemented tag)
  ($oops 'unimplemented (format "~a\n" tag)))

(define (emit-pb-binop-signal op op1 op2 flag result)
  (wasm-emit
    (local.get ,op1) 
    (local.get ,op2)
    ,(case op
      [(subz) `(
        (i64.sub)
        (local.set ,result)
        (local.get ,result)
        (i64.eqz)
        (local.set ,flag))]
      [(subp) `(
        (i64.sub)
        (i64.const 0)
        (i64.gt_s)
        (local.set ,flag)
        (local.set ,result))]
      [(sub) (do-pb-sub-pb-signal op1 op2 result flag)]
      [(mul) (do-pb-mul-pb-signal op1 op2 result flag)]
      [(add) (do-pb-add-pb-signal op1 op2 result flag)]
      [(div) (unimplemented 'div-signal)]
      [else (unimplemented op)])))

; wasm does not have a bitwise not instruction, but
;; (num_type.xor $a (num_type.const -1)) has the same effect, where -1 = 0xffffff..
(define-syntax wasm-not
  (syntax-rules ()
    [(_ x) `(i64.xor x (i64.const -1))]))

;; from pb.c: SIGN_FLIP(r, a, b) ((~((a ^ b) | (r ^ ~b))) >> (ptr_bits-1))
(define-syntax sign-flip
  (syntax-rules ()
    [(_ r a b)
      `(i64.shr_u
        ,(wasm-not 
          (i64.or
            (i64.xor a b)
              (i64.xor r ,(wasm-not b)))) (i64.const 63))]))

(define (do-pb-sub-pb-signal op1 op2 result flag)
  (wasm-emit
    (locals ([$a i64] [$b i64] [$r i64]))

    (local.set ,$r (i64.sub (local.get ,op1) (local.get,op2)))
    (local.set ,$a (local.get ,op1))
    (local.set ,$b (local.get ,op2))

    (local.set ,flag (i32.wrap_i64 ,(sign-flip (local.get ,$r) (local.get ,$a) (local.get ,$b))))
    (local.set ,result (local.get ,$r))))

(define (do-pb-add-pb-signal op1 op2 result flag)
  (wasm-emit
    (locals ([$a i64] [$b i64] [$r i64]))

    (local.set ,$r (i64.add (local.get ,op1) (local.get,op2)))
    (local.set ,$a (local.get ,op1))
    (local.set ,$b (local.get ,op2))

    (local.set ,flag (i32.wrap_i64 ,(sign-flip (local.get ,$r) (local.get ,$a) (local.get ,$b))))
    (local.set ,result (local.get ,$r))))

;; generates code to do a multiply-with-overflow-detect operation, assuming 
;; USE_OVERFLOW_INTRINSICS is disabled. See pb.h,
;; do_pb_bin_op_pb_signal_pb_mul_pb_register(dest, reg1, reg2)

;; To emulate the behavior when USE_OVERFLOW_INSTRINSICS is defined,
;; The Chez Scheme runtime function __builtin_mul_overflow
;; could be called (as pb.h does it), but that is an emscripten-compiled
;; C function, and I wanted to avoid generating wasm which had to use
;; emscripten calling conventions

(define (do-pb-mul-pb-signal op1 op2 result flag)
  (wasm-emit
    (locals ([$0 i64]))

    (local.set ,$0
      (i64.mul
        (local.get ,op1)
        (local.get ,op2)))
    
  
    (if (i64.eqz (local.get ,op2))
        ;; b == 0
        (then
            (local.set $flag (i32.const 0)))
        ;; b != 0
        (else
            ;; if b == -1
            (if (i64.eq (local.get ,op2) (i64.const -1))
                (then 
                    ;; flag = a != r * -1
                    (local.set ,flag
                        (i64.ne
                            (local.get ,op1) 
                            (i64.mul (local.get ,$0) (i64.const -1)))))
                (else
                    ;; flag = a != (signed) r / (signed b)
                    (local.set ,flag
                        (i64.ne
                            (local.get ,op1) 
                            (i64.sdiv (local.get ,$0) (local.get ,op2))))))))
    (local.set ,result (local.get ,$0))))
                            
(define (emit-pb-binop-signal-pb-register dest reg1 reg2 ms flag op)
    (wasm-emit
      (locals ([$0 i64]
                [$1 i64]
                [$2 i32]
                [$3 i64]))
      ,(load-from-reg reg1 ms $0) 
      ,(load-from-reg reg2 ms $1)
      ,(generate-regs-lhs dest ms 8 $2)
      ,(emit-pb-binop-signal op $0 $1 flag $3)

      (local.get ,$2)
      (local.get ,$3)

      (i64.store)))

(define (emit-pb-binop-signal-pb-immediate dest reg imm ms flag op)
    (wasm-emit
      (locals ([$0 i64]
                [$1 i64]
                [$2 i32]
                [$3 i64]))
      ,(load-from-reg reg ms $0)

      (i32.const ,imm)
      (i64.extend_i32_s)
      (local.set ,$1)

      ,(generate-regs-lhs dest ms 8 $2)
      ,(emit-pb-binop-signal op $0 $1 flag $3)

      (local.get ,$2)
      (local.get ,$3)

      (i64.store)))

(define (do-pb-cmp-op-no-signal op op1 op2)
  (wasm-emit
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
        (i64.eq))]
      [else (unimplemented op)])))

(define (emit-pb-cmp-op-pb-register reg1 reg2 ms cmp-op flag)
  (wasm-emit
    (locals ([$0 i64]
              [$1 i64]))
    ,(load-from-reg reg1 ms $0)
    ,(load-from-reg reg2 ms $1)
    ,(do-pb-cmp-op-no-signal cmp-op $0 $1)

    (local.set ,flag)))
  
(define (emit-pb-cmp-op-pb-immediate reg imm ms cmp-op flag)
  (wasm-emit
    (locals ([$0 i64] [$1 i64]))
    ,(load-from-reg reg ms $0)

    (i32.const ,imm)
    (i64.extend_i32_s)
    (local.set ,$1)

    ,(do-pb-cmp-op-no-signal cmp-op $0 $1)

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
    (locals ([$0 i32] [$1 i64]))
    ,(generate-regs-lhs dest ms word-size $0)

    ;; TODO: used passed in `ip` symbol
    (local.get $ip)
    (i32.const ,literal-offset)
    (i32.add)

    (,load-op)

    (local.set ,$1)

    (local.get ,$0)
    (local.get ,$1)
    (,store-op)))

; attempts to translate a pb load type into a wasm load type, 
;; using the pb load variant and the wasm type of the destination
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

(define (pb-st-type->wasm-st-type src-type word-size)
  (case src-type
    [(int8) 'store8]
    [(int16) 'store16]
    [(int32) (if (equal? word-size 4) 'store 'store32)]
    [(int64) 
      (begin
        (unless (equal? word-size 8)
          ($oops 'wasm-emit "incompatible store type ~a for word size ~a" src-type word-size))
        'store)]
    [(single) 'store]
    [(double) 
      (begin
        (unless (equal? word-size 8)
          ($oops 'wasm-emit "incompatible store type ~a for word size ~a" src-type word-size))
        'store)]))

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
    (locals ([$0 i32] [$1 i64] [$2 ,dest-type]))
    ,(if (or (equal? dest-type 'f32) (equal? dest-type 'f64))
        (generate-fpregs-lhs dest ms word-size $0)
        (generate-regs-lhs dest ms word-size $0))
    ,(load-from-reg base ms $1)
    (local.get ,$1)
    (i32.wrap_i64)
    (i32.const ,imm)
    (i32.add)

    (,load-op)
    (local.set ,$2)

    (local.get ,$0)
    (local.get ,$2)

    (,store-op)))

(define (emit-pb-ld-pb-register dest base reg ms src-type word-size)
  (define dest-type (word-size->dest-type word-size (is-fp? src-type)))
  (define load-type (pb-load-type->wasm-load-type src-type dest-type))
  (define load-op
    (string->symbol (format "~a.~a"  dest-type load-type)))
  (define store-op (string->symbol (format "~a.store" dest-type)))

  (wasm-emit
    (locals ([$0 i32] [$1 i64] [$2 i64] [$3 ,dest-type]))
    ,(if (or (equal? dest-type 'f32) (equal? dest-type 'f64))
        (generate-fpregs-lhs dest ms word-size $0)
        (generate-regs-lhs dest ms word-size $0))
    ,(load-from-reg base ms $1)
    ,(load-from-reg reg ms $2)

    (local.get ,$1)
    (i32.wrap_i64)

    (local.get ,$2)
    (i32.wrap_i64)
    (i32.add)

    (,load-op)
    (local.set ,$3)

    (local.get ,$0)
    (local.get ,$3)

    (,store-op)))

(define (emit-pb-st-pb-immediate dest base imm ms src-type word-size)
  (define reg-type (word-size->dest-type word-size (is-fp? src-type)))
  (define st-type (pb-st-type->wasm-st-type src-type word-size))

  (define store-op (string->symbol (format "~a.~a" reg-type st-type)))

  (wasm-emit 
    (locals 
            ; value to store 
            ([$0 ,reg-type]
            ; base address
              [$1 i64]))
    
    ,(if (or (equal? reg-type 'f32) (equal? reg-type 'f64))
      (load-from-fpreg dest ms $0)
      (load-from-reg dest ms $0))
    
    ,(load-from-reg base ms $1)
    (local.get ,$1)
    (i32.wrap_i64)
    (i32.const ,imm)
    (i32.add)

    (local.get ,$0)

    (,store-op)))

(define (emit-pb-st-pb-register dest base reg ms src-type word-size)
  (define reg-type (word-size->dest-type word-size (is-fp? src-type)))
  (define st-type (pb-st-type->wasm-st-type src-type word-size))

  (define store-op (string->symbol (format "~a.~a" reg-type st-type)))

  (wasm-emit 
    (locals 
            ; value to store 
            ([$0 ,reg-type]
            ; base address
              [$1 i64]
              [$2 i64]))
    
    ,(if (or (equal? reg-type 'f32) (equal? reg-type 'f64))
      (load-from-fpreg dest ms $0)
      (load-from-reg dest ms $0))
    
    ,(load-from-reg base ms $1)
    ,(load-from-reg reg ms $2)

    (local.get ,$1)
    (i32.wrap_i64)

    (local.get ,$2)
    (i32.wrap_i64)
    (i32.add)

    (local.get ,$0)

    (,store-op)))

; branch indirect, immediate
(define (emit-pb-b*-pb-immediate base target-offset ms)
  (wasm-emit
    (locals ([$0 i64]))
    ,(load-from-reg base ms $0)

    (local.get ,$0)
    (i32.wrap_i64)
    (i32.const ,target-offset)
    (i32.add)

    ; load from target address, 
    ; and wrap into i32, as we are operating in 32-bit address space
    (i64.load)
    (i32.wrap_i64)

    (return)))

(define (emit-pb-b-pb-immediate target-offset next-instr ms test)
  (wasm-emit
    ,(if (null? test) '(i32.const 1) test)
    (if 
      (then
        (return (i32.add (local.get $ip)
                  (i32.const ,target-offset))))
      (else
        (return (i32.add 
                  (local.get $ip) 
                  (i32.const ,next-instr)))))))

(define (emit-pb-b-pb-register reg next-instr ms test)
  (wasm-emit
    ,(if (null? test) '(i32.const 1) test)
    (if 
      ,(append
        '(then)
          (wasm-emit
            (locals ([$0 i64]))
            ,(load-from-reg reg ms $0)
            (local.get ,$0)
            (i32.wrap_i64)
            (return)))
      (else (return (i32.add (local.get $ip) 
                      (i32.const ,next-instr)))))))

(define (emit-pb-adr dest ms imm next-instr)
  (wasm-emit 
    (locals ([$0 i32]
             [$1 i32]))
    ,(generate-regs-lhs dest ms 8 $0)

    (local.get $ip)
    (i32.const ,next-instr)
    (i32.add)

    (i32.const ,imm)
    (i32.const 2)
    (i32.shl)

    (i32.add)
    (local.set ,$1)

    (local.get ,$0)
    (local.get ,$1)

    ; address is unsigned, we do not want to sign-extend when
    ; converting to i64
    (i64.extend_i32_u)

    (i64.store)))

(define (format-with-newlines wasm-sexp)
  (fold-left 
    string-append
    ""
    (map (lambda (x) (format "\t~a\n" x)) wasm-sexp)))

;; returns whether an s-expression can be interpreted
;; as a wasm local definition,
;; assuming the local definition is valid here
(define (is-local-def? sexp)
  (and (pair? sexp) (equal? (car sexp) 'local)))

;; returns whether an s-expression can be interpreted as a wasm `if`
;; we assume the `if` is valid here
(define (is-if-form? sexp)
  (and (pair? sexp) (equal? (car sexp) 'if)))

;; returns then portion of wasm if, in s-expression form
(define (then-block wasm-if)
  (cadr wasm-if))

;; returns else portion of wasm if
(define (else-block wasm-if) 
  (if (pair? (cddr wasm-if))
    (caddr wasm-if)
    '()))

; "hoist" the locals out of the generated wasm, so
; that they can be declared at the beginning of a wasm function

; params:
;   wasm: an S-expression representing generated wasm, 
;         with (local ...) declarations interspersed
; return: (instrs, locals*), where locals* represents
;         a list of the local definitions for a given wasm fragment,
;         and instrs is a list of the instructions without declarations
(define (hoist-locals wasm)
  (let-values ([(new-wasm locals)
    (let traverse ([src-wasm wasm] [built-wasm '()] [locals '()])
      (cond 
          [(null? src-wasm) (values built-wasm locals)]
          [(is-local-def? (car src-wasm))
            ;; do not add locals to the new wasm tree, but collect as part of locals list
            (traverse (cdr src-wasm) built-wasm (cons (car src-wasm) locals))]
          [(is-if-form? (car src-wasm)) 
            ; recurse on if and else branches
            (let-values ([(built-then new-locals1) (traverse (then-block (car src-wasm)) '() '())]
                        [(built-else new-locals2) (traverse (else-block (car src-wasm)) '() '())])
              (traverse (cdr src-wasm)
                        (if (null? built-else) 
                          (append built-wasm `((if 
                                                ,built-then)))
                          (append built-wasm `((if 
                                                ,built-then
                                                ,built-else))))
                        (append locals new-locals1 new-locals2)))]
          ; anything that is not a local declaration is not touched
          [else (traverse (cdr src-wasm) (append built-wasm (list (car src-wasm))) locals)]))])
    (append locals new-wasm)))