;; pbchunk conversion uses the fasl parser from "strip.ss"; it mutates
;; code in the parsed structure to generate references to C chunks
;; that implement a shadow version of a chunk of bytecode instructions,
;; and then the printer of "strip.ss" is used to write the updated
;; fasl content

(let ()

  (include "strip-types.ss")
  (include "wasm-emit.ss")

  ;; for a wasm module, what should the memory size be configured as?
  (define module-mem-size 20000)

  ;; Holdover configuration option from original pbchunk. `#t` is not practial for the boot files,
  ;; but is needed for wasm to avoid dispatch to chunklets (or sub-chunks)
  (define one-chunklet-per-chunk? #t)

;; state for the chunk writer:
(define-record-type chunk-info
  (fields (mutable counter)
          seen
          code-op)
  (nongenerative))

;; A chunklet represents a potential entry point into a code
;; object. It may have a prefix before the entry point that
;; is not generated as in C. A code object can have multiple
;; chunklets or just one.
(define-record-type chunklet
  (fields i        ; code offset for this start of this chunklet
          start-i  ; code offset for the entry point (= end-i if there's no entry)
          end-i    ; code offset aftet the end of this chunklet
          uses-flag? ; does the chunklet involve def-use pair of the branch flag
          continue-only? ; only render if part of a larger chunk?
          relocs   ; list of offset, none before this chunklet but maybe some after
          headers  ; list of (cons offset size), none before this chunklet but maybe some after
          labels)  ; list of `label`s, none before this chunklet but maybe some after
  (nongenerative))

;; A label within a chunklet, especially for interior branches
(define-record-type label
  (fields to        ; the label offset
          min-from  ; earliest offset that jumps here
          max-from  ; latest offset that jumps here
          all-from) ; all offsets that jump here
  (nongenerative))

(define (write-chunk-registration output-port chunk-function-names)
  ;; write $_chunksig type and $chunks table declaration to module
  (put-string output-port
    (format
      "(type $_chunksig (func (param i32) (param i32) (result i32)))\n(table $chunks ~a funcref)"
      (length chunk-function-names)))
  (newline output-port)
  (let write-table-elems ([todo-fns chunk-function-names] [n 0])
    (unless (null? todo-fns)
        (fprintf output-port "~a\n" `(elem (i32.const ,n) ,(car todo-fns)))
      (write-table-elems (cdr todo-fns) (add1 n))))
  (newline output-port) 

  ;; write wasm_pbchunk_register function
  (put-string output-port "(func $wasm_pbchunk_register (export \"wasm_pbchunk_register\")\n")
  (let initialize-table ([todo-fns chunk-function-names] [n 0])
    (unless (null? todo-fns)
      (fprintf output-port "\t~a\n" `(table.set 0 (i32.const ,n) (ref.func ,(car todo-fns))))
      (initialize-table (cdr todo-fns) (add1 n))))
  (put-string output-port ")")
  (newline output-port)

  ;; write wasm_do_jump helper which takes an index and uses it to dispatch into the table
  ;; of wasm chunks; avoids linker issues when attempting to call into a handwritten wasm
  ;; table from a C file
  (put-string output-port
    "(func $wasm_do_jump (export \"wasm_do_jump\") (param $idx i32) (param $ms i32) (param $ip i64)(result i64)
		  (i64.extend_i32_u 
			  (call_indirect (type $_chunksig)
				  (local.get $ms)
				  (i32.wrap_i64 (local.get $ip))
				  (local.get $idx))))"))

(define (range n)
  (unless (and (integer? n) (>= n 0))
    ($oops "range: expected non-negative integer"))
    (let loop ([i 0])
      (cond 
        [(< i n) (cons i (loop (add1 i)))]
        [else '()])))

(define (fasl-wasm-pbchunk! who output-file-name only-funcs exclude-funcs start-index entry* handle-entry finish-fasl-update)
  ;; first print everything to a string port, and then
  ;; break up the string port into separate files
  (let-values ([(0-op get) (open-string-output-port)])
    (let* ([seen-table (make-eq-hashtable)]
           [end-index
            (let loop ([entry* entry*] [index start-index])
              (cond
                [(null? entry*)
                 index]
                [else
                 (handle-entry
                  (car entry*)
                  (lambda (write-k)
                    (loop (cdr entry*) index))
                  (lambda (situation x)
                    (loop (cdr entry*)
                          (search-pbchunk! x 0-op index seen-table only-funcs exclude-funcs))))]))]
           [input-port (open-string-input-port (get))])
      ;; before continuing, write out updated fasl:
      (finish-fasl-update)

      (let ()
        (define generated-chunk-names 
          (map (lambda (i) (format "$chunk_~a" i))
             (range end-index)))

        (define (call-with-file k)
          (let ([output-port ($open-file-output-port who output-file-name (file-options replace)
                                                    (buffer-mode block)
                                                    (native-transcoder))])
                 (on-reset
                  (delete-file output-file-name #f)
                  (on-reset
                   (close-port output-port)
                   (k output-port)))))

        (call-with-file
         (lambda (output-port)
                (put-string output-port
                  (format "(module\n\t(memory ~a)" module-mem-size))
                 (let chunk-loop ([n 0] [line (get-line input-port)])
                   (cond
                     [(eof-object? line)
                      ; close module
                      (write-chunk-registration output-port generated-chunk-names)
                      (newline output-port)
                      (put-string output-port ")")
                      (close-port output-port)]
                     [else
                      (put-string output-port line)
                      (newline output-port)
                      (chunk-loop n (get-line input-port))])))))
        ;; files written; return index after last chunk
        end-index)))

;; The main pbchunk handler: takes a fasl object in "strip.ss" form,
;; find code objects inside, and potentially generates chunks and updates
;; the code object with references to chunks. Takes the number of
;; chunks previously written and returns the total number written after.
(define (search-pbchunk! v code-op start-index seen-table only-funcs exclude-funcs)
  (let ([ci (make-chunk-info start-index
                             seen-table
                             code-op)])
    (chunk! v ci only-funcs exclude-funcs)
    (chunk-info-counter ci)))

(define (chunk! v ci only-funcs exclude-funcs)
  (unless (eq-hashtable-ref (chunk-info-seen ci) v #f)
    (eq-hashtable-set! (chunk-info-seen ci) v #t)
    (do-chunk! v ci only-funcs exclude-funcs)))

(define (chunk-vector! vec ci only-funcs exclude-funcs)
  (vector-for-each (lambda (e) (chunk! e ci only-funcs exclude-funcs)) vec))

(define (do-chunk! v ci only-funcs exclude-funcs)
  (fasl-case* v
    [(pair vec)
     (chunk-vector! vec ci only-funcs exclude-funcs)]
    [(tuple ty vec)
     (constant-case* ty
       [(fasl-type-box fasl-type-immutable-box)
        (chunk! (vector-ref vec 0) ci only-funcs exclude-funcs)]
       [(fasl-type-weak-pair)
        ($oops 'chunk "weak pair not supported")]
       [(fasl-type-ephemeron)
        ($oops 'chunk "ephemeron pair not supported")]
       [else (void)])]
    [(vector ty vec)
     (constant-case* ty
       [(fasl-type-vector fasl-type-immutable-vector)
        (chunk-vector! vec ci only-funcs exclude-funcs)]
       [else (void)])]
    [(stencil-vector mask vec sys?)
     (chunk-vector! vec ci only-funcs exclude-funcs)]
    [(record maybe-uid size nflds rtd pad-ty* fld*)
     (for-each (lambda (fld)
                 (field-case fld [ptr (elem) (chunk! elem ci only-funcs exclude-funcs)] [else (void)]))
               fld*)]
    [(closure offset c)
     (chunk! c ci only-funcs exclude-funcs)]
    [(code flags free name arity-mask info pinfo* bytes m vreloc)
     (chunk-code! name bytes vreloc ci only-funcs exclude-funcs)
     (chunk-vector! vreloc ci only-funcs exclude-funcs)]
    [(reloc type-etc code-offset item-offset elem)
     (chunk! elem ci only-funcs exclude-funcs)]
    [(symbol-hashtable mutable? minlen subtype veclen vpfasl)
     (vector-for-each (lambda (p)
                        (chunk! (car p) ci only-funcs exclude-funcs)
                        (chunk! (cdr p) ci only-funcs exclude-funcs))
                      vpfasl)]
    [(indirect g i) (chunk! (vector-ref g i) ci only-funcs exclude-funcs)]
    [else
     ;; nothing else contains references that can reach code
     (void)]))

(define min-chunk-len 3)
(define instr-bytes 4)
(define reloc-instrs 4)

(define (instr-op instr) (bitwise-and instr #xFF))

(define (instr-d-dest instr) (bitwise-and (bitwise-arithmetic-shift-right instr 8) #xF))

(define (instr-dr-dest instr) (instr-d-dest instr))
(define (instr-dr-reg instr) (bitwise-and (bitwise-arithmetic-shift-right instr 16) #xF))

(define (instr-di-dest instr) (instr-d-dest instr))
(define (instr-di-imm instr) (bitwise-arithmetic-shift-right instr 16))

(define (instr-adr-dest instr) (instr-di-dest instr))
(define (instr-adr-imm instr) (bitwise-arithmetic-shift-right instr 12))

(define (instr-drr-dest instr) (instr-d-dest instr))
(define (instr-drr-reg1 instr) (bitwise-and (bitwise-arithmetic-shift-right instr 12) #xF))
(define (instr-drr-reg2 instr) (bitwise-and (bitwise-arithmetic-shift-right instr 16) #xF))

(define (instr-dri-dest instr) (instr-d-dest instr))
(define (instr-dri-reg instr) (bitwise-and (bitwise-arithmetic-shift-right instr 12) #xF))
(define (instr-dri-imm instr) (bitwise-arithmetic-shift-right instr 16))

(define (instr-i-imm instr) (bitwise-arithmetic-shift-right instr 8))

(define (make-chunk-instr index sub-index)
  (unless (eqv? index (bitwise-and index #xFFFF))
    ($oops 'pbchunk "chunk index ~a is too large" index))
  (unless (eqv? sub-index (bitwise-and sub-index #xFF))
    ($oops 'pbchunk "chunk sub-index ~a is too large" sub-index))
  (bitwise-ior (constant pb-chunk)
               (bitwise-arithmetic-shift-left sub-index 8)
               (bitwise-arithmetic-shift-left index 16)))

(define (make-wasm-chunk-instr index sub-index)
  (unless (eqv? index (bitwise-and index #xFFFF))
    ($oops 'pbchunk "chunk index ~a is too large" index))
  (unless (eqv? sub-index (bitwise-and sub-index #xFF))
    ($oops 'pbchunk "chunk sub-index ~a is too large" sub-index))
    ;; TODO: add as constant
  (bitwise-ior 229
               (bitwise-arithmetic-shift-left sub-index 8)
               (bitwise-arithmetic-shift-left index 16)))

(define MAX-SUB-INDEXES 256)

;; expands to a binary search for the right case
(define-syntax (instruction-case stx)
  (syntax-case stx ()
    [(_ instr emit [op . shape] ...)
     (let ([vec (make-vector 256 0)]
           [emits (list->vector #'(($oops 'chunk "unrecognized instruction ~s" instr)
                                   (emit op . shape) ...))])
       (let loop ([ops (datum (op ...))] [pos 1])
         (unless (null? ops)
           (vector-set! vec (lookup-constant (car ops)) pos)
           (loop (cdr ops) (fx+ pos 1))))
       #`(let ([pos (vector-ref '#,vec (instr-op instr))])
           #,(let loop ([start 0] [end (vector-length emits)])
               (cond
                 [(fx= (fx+ start 1) end)
                  (vector-ref emits start)]
                 [else
                  (let ([mid (quotient (+ start end) 2)])
                    #`(if (fx>= pos #,mid)
                          #,(loop mid end)
                          #,(loop start mid)))]))))]))

;; di = destination register and immediate
;; dr = destination register and immediate
;; etc.
;; .../x = not handled, so return to interpret
;; .../u = unsigned immediate
;; .../f = sets flag
;; .../b = branch, uses flag deending on branch kind
;; .../c = foreign call
(define-syntax (instruction-cases stx)
  (syntax-case stx ()
    [(_ instr emit)
     #'(instruction-case
        instr emit
        ;; every instruction implemented in "pb.c" needs to be here,
        ;; except for the `pb-chunk` instruction
        [pb-nop nop]
        [pb-literal literal]
        [pb-mov16-pb-zero-bits-pb-shift0 di/u mov16/z shift0]
        [pb-mov16-pb-zero-bits-pb-shift1 di/u mov16/z shift1]
        [pb-mov16-pb-zero-bits-pb-shift2 di/u mov16/z shift2]
        [pb-mov16-pb-zero-bits-pb-shift3 di/u mov16/z shift3]
        [pb-mov16-pb-keep-bits-pb-shift0 di/u mov16/k shift0]
        [pb-mov16-pb-keep-bits-pb-shift1 di/u mov16/k shift1]
        [pb-mov16-pb-keep-bits-pb-shift2 di/u mov16/k shift2]
        [pb-mov16-pb-keep-bits-pb-shift3 di/u mov16/k shift3]
        [pb-mov-pb-i->i dr mov i->i]
        [pb-mov-pb-d->d dr mov d->d]
        [pb-mov-pb-i->d dr mov i->d]
        [pb-mov-pb-d->i dr mov d->i]
        [pb-mov-pb-s->d dr mov s->d]
        [pb-mov-pb-d->s dr mov d->s]
        [pb-mov-pb-d->s->d dr mov d->s->d]
        [pb-mov-pb-i-bits->d-bits dr mov i-bits->d-bits]
        [pb-mov-pb-d-bits->i-bits dr mov d-bits->i-bits]
        [pb-mov-pb-i-i-bits->d-bits drr mov i-i-bits->d-bits]
        [pb-mov-pb-d-lo-bits->i-bits dr mov d-lo-bits->i-bits]
        [pb-mov-pb-d-hi-bits->i-bits dr mov d-hi-bits->i-bits]
        [pb-bin-op-pb-no-signal-pb-add-pb-register drr binop add]
        [pb-bin-op-pb-no-signal-pb-add-pb-immediate dri binop add]
        [pb-bin-op-pb-no-signal-pb-sub-pb-register drr binop sub]
        [pb-bin-op-pb-no-signal-pb-sub-pb-immediate dri binop sub]
        [pb-bin-op-pb-no-signal-pb-mul-pb-register drr binop mul]
        [pb-bin-op-pb-no-signal-pb-mul-pb-immediate dri binop mul]
        [pb-bin-op-pb-no-signal-pb-div-pb-register drr binop div]
        [pb-bin-op-pb-no-signal-pb-div-pb-immediate dri binop div]
        [pb-bin-op-pb-no-signal-pb-and-pb-register drr binop and]
        [pb-bin-op-pb-no-signal-pb-and-pb-immediate dri binop and]
        [pb-bin-op-pb-no-signal-pb-ior-pb-register drr binop ior]
        [pb-bin-op-pb-no-signal-pb-ior-pb-immediate dri binop ior]
        [pb-bin-op-pb-no-signal-pb-xor-pb-register drr binop xor]
        [pb-bin-op-pb-no-signal-pb-xor-pb-immediate dri binop xor]
        [pb-bin-op-pb-no-signal-pb-lsl-pb-register drr binop lsl]
        [pb-bin-op-pb-no-signal-pb-lsl-pb-immediate dri binop lsl]
        [pb-bin-op-pb-no-signal-pb-lsr-pb-register drr binop lsr]
        [pb-bin-op-pb-no-signal-pb-lsr-pb-immediate dri binop lsr]
        [pb-bin-op-pb-no-signal-pb-asr-pb-register drr binop asr]
        [pb-bin-op-pb-no-signal-pb-asr-pb-immediate dri binop asr]
        [pb-bin-op-pb-no-signal-pb-lslo-pb-register drr binop lslo]
        [pb-bin-op-pb-no-signal-pb-lslo-pb-immediate dri binop lslo]
        [pb-bin-op-pb-signal-pb-add-pb-register drr/f binop add]
        [pb-bin-op-pb-signal-pb-add-pb-immediate dri/f binop add]
        [pb-bin-op-pb-signal-pb-sub-pb-register drr/f binop sub]
        [pb-bin-op-pb-signal-pb-sub-pb-immediate dri/f binop sub]
        [pb-bin-op-pb-signal-pb-mul-pb-register drr/f binop mul]
        [pb-bin-op-pb-signal-pb-mul-pb-immediate dri/f binop mul]
        [pb-bin-op-pb-signal-pb-subz-pb-register drr/f binop subz]
        [pb-bin-op-pb-signal-pb-subz-pb-immediate dri/f binop subz]
        [pb-bin-op-pb-signal-pb-subp-pb-register drr/f binop subp]
        [pb-bin-op-pb-signal-pb-subp-pb-immediate dri/f binop subp]
        [pb-cmp-op-pb-eq-pb-register dr/f eq]
        [pb-cmp-op-pb-eq-pb-immediate di/f eq]
        [pb-cmp-op-pb-lt-pb-register dr/f lt]
        [pb-cmp-op-pb-lt-pb-immediate di/f lt]
        [pb-cmp-op-pb-gt-pb-register dr/f gt]
        [pb-cmp-op-pb-gt-pb-immediate di/f gt]
        [pb-cmp-op-pb-le-pb-register dr/f le]
        [pb-cmp-op-pb-le-pb-immediate di/f le]
        [pb-cmp-op-pb-ge-pb-register dr/f ge]
        [pb-cmp-op-pb-ge-pb-immediate di/f ge]
        [pb-cmp-op-pb-ab-pb-register dr/f ab]
        [pb-cmp-op-pb-ab-pb-immediate di/f ab]
        [pb-cmp-op-pb-bl-pb-register dr/f bl]
        [pb-cmp-op-pb-bl-pb-immediate di/f bl]
        [pb-cmp-op-pb-cs-pb-register dr/f cs]
        [pb-cmp-op-pb-cs-pb-immediate di/f cs]
        [pb-cmp-op-pb-cc-pb-register dr/f cc]
        [pb-cmp-op-pb-cc-pb-immediate di/f cc]
        [pb-fp-bin-op-pb-add-pb-register fp-drr add]
        [pb-fp-bin-op-pb-sub-pb-register fp-drr sub ]
        [pb-fp-bin-op-pb-mul-pb-register fp-drr mul]
        [pb-fp-bin-op-pb-div-pb-register fp-drr div]
        [pb-un-op-pb-not-pb-register dr not]
        [pb-un-op-pb-not-pb-immediate di not]
        [pb-fp-un-op-pb-sqrt-pb-register fp-dr sqrt]
        [pb-fp-cmp-op-pb-eq-pb-register fp-dr/f eq]
        [pb-fp-cmp-op-pb-lt-pb-register fp-dr/f lt]
        [pb-fp-cmp-op-pb-le-pb-register fp-dr/f le]
        [pb-rev-op-pb-int16-pb-register dr rev int16]
        [pb-rev-op-pb-uint16-pb-register dr rev uint16 ]
        [pb-rev-op-pb-int32-pb-register dr rev int32]
        [pb-rev-op-pb-uint32-pb-register dr rev uint32]
        [pb-rev-op-pb-int64-pb-register dr rev int64]
        [pb-ld-op-pb-int8-pb-register drr ld int8]
        [pb-ld-op-pb-int8-pb-immediate dri ld int8]
        [pb-ld-op-pb-uint8-pb-register drr ld uint8]
        [pb-ld-op-pb-uint8-pb-immediate dri ld uint8]
        [pb-ld-op-pb-int16-pb-register drr ld int16]
        [pb-ld-op-pb-int16-pb-immediate dri ld int16]
        [pb-ld-op-pb-uint16-pb-register drr ld uint16]
        [pb-ld-op-pb-uint16-pb-immediate dri ld uint16]
        [pb-ld-op-pb-int32-pb-register drr ld int32]
        [pb-ld-op-pb-int32-pb-immediate dri ld int32]
        [pb-ld-op-pb-uint32-pb-register drr ld uint32]
        [pb-ld-op-pb-uint32-pb-immediate dri ld uint32]
        [pb-ld-op-pb-int64-pb-register drr ld int64]
        [pb-ld-op-pb-int64-pb-immediate dri ld int64]
        [pb-ld-op-pb-double-pb-register drr ld double]
        [pb-ld-op-pb-double-pb-immediate dri ld double]
        [pb-ld-op-pb-single-pb-register drr ld single]
        [pb-ld-op-pb-single-pb-immediate dri ld single]
        [pb-st-op-pb-int8-pb-register drr st int8]
        [pb-st-op-pb-int8-pb-immediate dri st int8]
        [pb-st-op-pb-int16-pb-register drr st int16]
        [pb-st-op-pb-int16-pb-immediate dri st int16]
        [pb-st-op-pb-int32-pb-register drr st int32]
        [pb-st-op-pb-int32-pb-immediate dri st int32]
        [pb-st-op-pb-int64-pb-register drr st int64]
        [pb-st-op-pb-int64-pb-immediate dri st int64]
        [pb-st-op-pb-double-pb-register drr st double]
        [pb-st-op-pb-double-pb-immediate dri st double]
        [pb-st-op-pb-single-pb-register drr st single]
        [pb-st-op-pb-single-pb-immediate dri st single]
        [pb-b-op-pb-fals-pb-register r/b '(i32.eq (local.get $flag) (i32.const 0))]
        [pb-b-op-pb-fals-pb-immediate i/b '(i32.eq (local.get $flag) (i32.const 0))]
        [pb-b-op-pb-true-pb-register r/b '(local.get $flag)]
        [pb-b-op-pb-true-pb-immediate i/b '(local.get $flag)]
        [pb-b-op-pb-always-pb-register r/b '()]
        [pb-b-op-pb-always-pb-immediate i/b '()]
        [pb-b*-op-pb-register dr/b]
        [pb-b*-op-pb-immediate di/b]
        [pb-return n/x]
        [pb-adr adr]
        [pb-interp d/x]
        [pb-call dri/c]
        [pb-inc-pb-register dr/f]
        [pb-inc-pb-immediate di/f]
        [pb-lock d/f]
        [pb-cas drr/f]
        [pb-fence-pb-fence-store-store n]
        [pb-fence-pb-fence-acquire n]
        [pb-fence-pb-fence-release n]
        [pb-call-arena-in di]
        [pb-fp-call-arena-in di]
        [pb-call-arena-out di]
        [pb-fp-call-arena-out di]
        [pb-stack-call dr])]))

(define (advance l sel i)
  (let loop ([l l])
    (cond
      [(null? l) '()]
      [(fx>= (sel (car l)) i) l]
      [else (loop (cdr l))])))

(define (advance-relocs relocs i)
  (advance relocs values i))

(define (advance-headers headers i)
  (advance headers car i))

(define (advance-labels labels i)
  (advance labels label-to i))

(define (ensure-label i labels)
  (cond
    [(and (pair? labels)
          (fx= i (label-to (car labels))))
     (let ([l (car labels)])
       (cons (make-label i
                         (fxmin i (label-min-from l))
                         (fxmax i (label-max-from l))
                         (cons i (label-all-from l)))
             (cdr labels)))]
    [(and (pair? labels)
          (fx> i (label-to (car labels))))
     (cons (car labels) (ensure-label i (cdr labels)))]
    [else
     (cons (make-label i i i (list i))
           labels)]))

(define (sort-and-combine-labels labels)
  (let ([labels (sort (lambda (a b) (< (label-to a) (label-to b))) labels)])
    (let remove-dups ([labels labels])
      (cond
        [(null? labels) '()]
        [(null? (cdr labels)) labels]
        [else
         (let ([a (car labels)]
               [b (cadr labels)])
           (if (fx= (label-to a) (label-to b))
               (remove-dups (cons (make-label (label-to a)
                                              (fxmin (label-min-from a)
                                                     (label-min-from b))
                                              (fxmax (label-max-from a)
                                                     (label-max-from b))
                                              (append (label-all-from a)
                                                      (label-all-from b)))
                                  (cddr labels)))
               (cons a (remove-dups (cdr labels)))))]))))

(define (empty-chunklet? c)
  (or (fx= (chunklet-start-i c)
           (chunklet-end-i c))
      (chunklet-continue-only? c)))

(define (is-comment? wasm-sexp)
  (and (pair? wasm-sexp) (equal? (car wasm-sexp) 'comment)))

(define (comment-string wasm-comment) (cdr wasm-comment))

(define (write-compiled-wasm o wasm)
  (let write-loop ([wasm wasm])
    (unless (null? wasm)
      (let ([cur (car wasm)])
      (cond
        [(is-comment? cur) (printf "~s\n" cur) (fprintf o "\t~a\n" (comment-string cur))]
        [else (fprintf o "\t~a\n" cur)]))
          (write-loop (cdr wasm)))))

;; Found a code object, maybe generate a chunk
(define (chunk-code! name bv vreloc ci only-funcs exclude-funcs)
  (let ([len (bytevector-length bv)]
        [o (chunk-info-code-op ci)]
        [relocs (let loop ([off 0] [rels (vector->list vreloc)])
                  (cond
                    [(null? rels) '()]
                    [else
                     (fasl-case* (car rels)
                       [(reloc type-etc code-offset item-offset elem)
                        (let ([off (+ off code-offset)])
                          (cons (fx- off (constant code-data-disp))
                                (loop off (cdr rels))))]
                       [else '()])]))]
        [name (extract-name name)])
        
    (fprintf o "\n;; code ~a \n" name)
    (unless (or (equal? name "winder-dummy") 
                (if (pair? only-funcs) (not (member name only-funcs)) #f)
                (if (pair? exclude-funcs) (member name exclude-funcs)  #f)) ; hack to avoid special rp header in dounderflow
      (display (format "name: ~a\n" name))
      (let ([chunklets
             ;; use `select-instruction-range` to partition the code into chunklets
             (let-values ([(headers labels) (gather-targets bv len)])
               #;(fprintf o "/* labels: ~s */\n" (map (lambda (l) (format "0x~x" (label-to l))) labels))
               (let loop ([i 0] [relocs relocs] [headers headers] [labels labels])
                 (cond
                   [(fx= i len) '()]
                   [else
                    (let-values ([(start-i end-i uses-flag?)
                                  (select-instruction-range bv i len relocs headers labels)])
                      (display (format "instruction range for chunk: ~a ~a\n" start-i end-i))
                      (when (fx= i end-i)
                        ($oops 'chunk-code "failed to make progress at ~a out of ~a" i len))
                      (let ([continue-only? #f])
                             ;; when the chunk would be too small to save us any time, so don't
                             ;; bother make it stand-alone; a threshold greater than 1 also avoids
                             ;; code that wouldn't even use `ms` or `ip`:
                            ;  (fx< (fx- end-i start-i)
                            ;       (fx* min-chunk-len instr-bytes))])
                        (cons (make-chunklet i start-i end-i uses-flag? continue-only? relocs headers labels)
                              (loop end-i
                                    (advance-relocs relocs end-i)
                                    (advance-headers headers end-i)
                                    (advance-labels labels end-i)))))])))]
            [index (chunk-info-counter ci)])
        ;; We can either generate each chunklet as its own chunk
        ;; function or generate one chunk function with multiple
        ;; chunklets
        (let ([count (fold-left (lambda (sum c) (if (empty-chunklet? c) sum (fx+ 1 sum)))
                                0
                                chunklets)])
          (cond
            ; [(fx> count 256)
            ;  ;; this many chunklets suggests that compilation is not productive,
            ;  ;; so just show the disassembly
            ;  (fprintf o "/* (too many entry points) */\n")
            ;  (let ([all-chunklets chunklets])
            ;    (let loop ([chunklets chunklets])
            ;      (unless (null? chunklets)
            ;        (let ([c (car chunklets)])
            ;          (compile-chunklet o bv
            ;                         (chunklet-i c) 0
            ;                         (chunklet-relocs c) (chunklet-headers c) '()
            ;                         (chunklet-end-i c) ; => treat as empty
            ;                         (chunklet-end-i c)
            ;                         all-chunklets
            ;                         ;; fallthrough?
            ;                         #t)
            ;          (loop (cdr chunklets))))))]
            [(or one-chunklet-per-chunk?
                 ;; also use this more if there's 0 or 1 chunklets to emit,
                 ;; or more than `MAX-SUB-INDEXES`:
                 (let ()
                   (or (fx< count 2)
                       (fx> count MAX-SUB-INDEXES))))
             (let loop ([chunklets chunklets] [index index])
               (cond
                 [(null? chunklets)
                  (chunk-info-counter-set! ci index)]
                 [else
                  (let ([c (car chunklets)])
                    ;; generate a non-empty chunk as its own function
                    (unless (empty-chunklet? c)
                      (emit-wasm-chunk-header o index #f (chunklet-uses-flag? c)))
                    
                    (unless (empty-chunklet? c)
                      (display (format "chunklet-i: ~a; chunklet-start-i: ~a; chunklet-end-i: ~a\n"
                        (chunklet-i c) (chunklet-start-i c) (chunklet-end-i c))))

                    (let ([compiled-wasm 
                            (compile-chunklet o bv
                                   (chunklet-i c) (chunklet-start-i c)
                                   (chunklet-relocs c) (chunklet-headers c) (chunklet-labels c)
                                   (if (chunklet-continue-only? c)
                                       (chunklet-end-i c)
                                       (chunklet-start-i c))
                                   (chunklet-end-i c)
                                   (list c) ; `goto` branches contrained to this chunklet
                                   ;; fallthrough?
                                   (empty-chunklet? c))]) 
                                   (write-compiled-wasm o (hoist-locals compiled-wasm)))

                    (unless (empty-chunklet? c)
                      (emit-wasm-chunk-footer o)
                      (bytevector-u32-set! bv (chunklet-start-i c) (make-wasm-chunk-instr index 0) (constant fasl-endianness)))
                    (loop (cdr chunklets) (if (empty-chunklet? c) index (fx+ index 1))))]))]
            [else
             ;; one chunk for the whole code object, where multiple entry points are
             ;; supported by a sub-index
             (emit-wasm-chunk-header o index #t (ormap chunklet-uses-flag? chunklets))
             (chunk-info-counter-set! ci (fx+ 1 index))
             ;; dispatch to label on entry via sub-index
             (fprintf o "  switch (sub_index) {\n")
             ;; dispatch to a chunklet 
             (let loop ([chunklets chunklets] [sub-index 0])
               (unless (null? chunklets)
                 (let ([c (car chunklets)])
                   (cond
                     [(empty-chunklet? c) (loop (cdr chunklets) sub-index)]
                     [else
                      (fprintf o "    case ~a:~a ip -= 0x~x; goto label_~x;\n"
                               sub-index
                               (if (andmap empty-chunklet? (cdr chunklets))
                                   " default:"
                                   "")
                               (chunklet-start-i c)
                               (chunklet-start-i c))
                      (loop (cdr chunklets) (fx+ 1 sub-index))]))))
             (fprintf o "  }\n")
             (let ([all-chunklets chunklets])
               (let loop ([chunklets chunklets] [sub-index 0])
                 (unless (null? chunklets)
                   (let ([c (car chunklets)])
                     ;; emit a chunklet within the function
                     (compile-chunklet o bv
                                    (chunklet-i c) 0
                                    (chunklet-relocs c) (chunklet-headers c)
                                    (if (empty-chunklet? c)
                                        (chunklet-labels c)
                                        (ensure-label (chunklet-start-i c) (chunklet-labels c)))
                                    (chunklet-start-i c) (chunklet-end-i c)
                                    all-chunklets ; `goto` branches allowed across chunklets
                                    ;; fallthrough?
                                    (and (pair? (cdr chunklets))
                                         (fx= (chunklet-end-i c)
                                              (chunklet-start-i (cadr chunklets)))))
                     (unless (empty-chunklet? c)
                       (bytevector-u32-set! bv (chunklet-start-i c) (make-chunk-instr index sub-index) (constant fasl-endianness)))
                     (loop (cdr chunklets) (if (empty-chunklet? c) sub-index (fx+ 1 sub-index)))))))
             (emit-wasm-chunk-footer o)]))))))

;; Find all branch targets in the code object
(define (gather-targets bv len)
  (let loop ([i 0] [headers '()] [labels '()])
    (cond
      [(fx= i len) (values '() (sort-and-combine-labels labels))]
      [(and (pair? headers)
            (fx= i (caar headers)))
       (let ([size (cdar headers)])
         (let ([i (+ i size)])
           (let-values ([(rest-headers labels) (loop i (cdr headers) labels)])
             (values (cons (car headers) rest-headers)
                     labels))))]
      [else
       (let ([instr (bytevector-s32-ref bv i (constant fasl-endianness))]
             [uinstr (bytevector-u32-ref bv i (constant fasl-endianness))])
         (define (next)
           (loop (fx+ i instr-bytes) headers labels))

         (define (next/add-label new-label)
           (loop (fx+ i instr-bytes) headers (cons new-label labels)))

         (define (next/adr)
           (let ([delta (fx* instr-bytes (instr-adr-imm instr))])
             (cond
               [(> delta 0)
                (let* ([after (fx+ i instr-bytes delta)]
                       [size (if (fx= 1 (fxand 1 (bytevector-u8-ref bv (fx- after
                                                                            (if (eq? (constant fasl-endianness) 'little)
                                                                                (constant ptr-bytes)
                                                                                1)))))
                                 (constant size-rp-compact-header)
                                 (constant size-rp-header))]
                       [start (fx- after size)]
                       [header (cons start size)])
                  (loop (fx+ i instr-bytes)
                        ;; insert keeping headers sorted
                        (let loop ([headers headers])
                          (cond
                            [(null? headers) (list header)]
                            [(fx<= start (caar headers)) (cons header headers)]
                            [else (cons (car headers) (loop (cdr headers)))]))
                        labels))]
               [else (next)])))

         (define (next-branch)
           (let* ([delta (instr-i-imm instr)]
                  [target-label (fx+ i instr-bytes delta)])
             (next/add-label (make-label target-label i i (list i)))))

         (define (next/literal)
           (loop (fx+ i instr-bytes (constant ptr-bytes)) headers labels))

         (define-syntax (dispatch stx)
           (syntax-case stx (i/b adr literal)
             [(_ op i/b test) #'(next-branch)]
             [(_ op adr) #'(next/adr)]
             [(_ op literal) #'(next/literal)]
             [_ #'(next)]))

         (instruction-cases instr dispatch))])))

;; want to select only chunks that have supported instructions in them
;; additional chunking rules should stay in place, but we need to
;; stop chunks anytime there is a branch

;; Select next chunklet within a code object
(define (select-instruction-range bv i len relocs headers labels)
  (let loop ([i i] [relocs relocs] [headers headers] [labels labels] [start-i #f]
             [flag-ready? #f] [uses-flag? #f])
    (cond
      ; base case -- end selection range if we hit length of code bytes
      [(fx= i len) (values (or start-i i) i uses-flag?)]

        ; if we have headers, and current instruction offset
        ; is equal to start index of first header.. 
      [(and (pair? headers)
            (fx= i (caar headers)))
       (cond
         [start-i
          ;; we want to start a new chunk after the header, so end this one
          (values start-i i uses-flag?)]
         [else ; start-i is non-zero, so advance current offset i by
               ; the size of the header whose start index we match
          (let* ([size (cdar headers)]
                 [i (+ i size)])
            (loop i
                  (advance-relocs relocs i)
                  (cdr headers)
                  labels
                  start-i
                  #f
                  uses-flag?))])]
      [(and (pair? labels)
            (fx= i (label-to (car labels))))
       ;; we want to stop at this label if it's a target outside the range
       ;; that we're trying to build
       (cond
         [(< (label-min-from (car labels)) (or start-i i))
          ;; target from jump before this chunk
          (if start-i
              (values start-i i uses-flag?)
              (loop i relocs headers (cdr labels) #f #f uses-flag?))]
         [(< (label-max-from (car labels)) i)
          ;; always a forward jump within this chunk
          (loop i relocs headers (cdr labels) start-i #f uses-flag?)]
         [else
          ;; some backward jump exists, but tenatively assume that
          ;; it's within the chunk, then check;
          ;; WARNING: this makes overall chunking not linear-time, but
          ;; it's probably ok in practice
          (let-values ([(maybe-start-i end-i maybe-uses-flag?)
                        (loop i relocs headers (cdr labels) start-i #f uses-flag?)])
            (cond
              [(fx>= maybe-start-i i)
               ;; chunk here or starts later, anyway
               (values maybe-start-i end-i maybe-uses-flag?)]
              [(fx< (label-max-from (car labels)) end-i)
               ;; backward jumps stay within chunk
               (values maybe-start-i end-i maybe-uses-flag?)]
              [else
               ;; not within chunk
               (values start-i i uses-flag?)]))])]
      [(and (pair? relocs)
            (fx>= i (car relocs)))
       ($oops 'pbchunk "landed at a relocation")]
      [else
       ;; if the instruction always has to trampoline back, then the instruction
       ;; after can start a chunk to resume
       (let ([instr (bytevector-s32-ref bv i (constant fasl-endianness))])
         (define (check-flag)
           (unless flag-ready?
             ($oops 'pbchunk "branch not immediately after signal at 0x~x" i)))
         (define (keep now-uses-flag?)
           (when now-uses-flag? (check-flag))
           (loop (fx+ i instr-bytes) relocs headers labels (or start-i i) #f (or uses-flag?
                                                                                 now-uses-flag?)))
         (define (keep-and-stop-after now-uses-flag?)
           (when now-uses-flag? (check-flag))
           (values (or start-i i) (fx+ i instr-bytes) (or uses-flag?
                                                          now-uses-flag?)))

         (define (keep-signalling)
           (loop (fx+ i instr-bytes) relocs headers labels (or start-i i) #t uses-flag?))

         (define (skip)
           (loop (fx+ i instr-bytes) relocs headers labels (or start-i i) flag-ready? uses-flag?))

         (define (stop-before)
           (if start-i
               (values start-i i uses-flag?)
               (loop (fx+ i instr-bytes) relocs headers labels #f #f uses-flag?)))

         (define (stop-after)
           (values (or start-i i) (fx+ i instr-bytes) uses-flag?))

         (define (keep-literal)
           (unless (and (pair? relocs)
                        (fx= (fx+ i instr-bytes) (car relocs)))
             ($oops 'pbchunk "no relocation after pb-literal"))
           (let ([next-i (fx+ i instr-bytes (constant ptr-bytes))])
             (loop next-i (cdr relocs) headers labels (or start-i i) #f uses-flag?)))

         (define-syntax (dispatch stx)
           (syntax-case stx (dri/x dr/x d/x n/x r/b i/b d/f dr/b di/b
                                   dr/f fp-dr/f di/f drr/f dri/f literal nop)
             [(_ op dri/x) #'(stop-before)]
             [(_ op dr/x) #'(stop-before)]
             [(_ op d/x) #'(stop-before)]
             [(_ op n/x) #'(stop-before)]
             [(_ op r/b '()) #'(keep-and-stop-after #f)]
             [(_ op i/b '()) #'(keep-and-stop-after #f)]
             [(_ op r/b _) #'(keep-and-stop-after #t)]
             [(_ op i/b _) #'(keep-and-stop-after #t)]
             [(_ op fp-dr/f _) #'(keep-signalling)]
             [(_ op d/f) #'(keep-signalling)]
             [(_ op dr/b . _) #'(stop-after)]
             [(_ op di/b . _) #'(stop-after)]
             [(_ op dr/f . _) #'(keep-signalling)]
             [(_ op di/f . _) #'(keep-signalling)]
             [(_ op drr/f . _) #'(keep-signalling)]
             [(_ op dri/f . _) #'(keep-signalling)]
             [(_ op literal) #'(keep-literal)]
             ;[(_ op nop) #'(keep-signalling)] ; nops are occasionally emitted between comparison and branch instructions
             [_ #'(skip)]))
         (instruction-cases instr dispatch))])))

(define (emit-wasm-chunk-header o index sub-index? uses-flag?)
  (fprintf o 
    "(func $chunk_~a (export \"chunk_~a\")
      (param $ms i32)
      (param $ip i32)
      (result i32)
      ~a"
        index 
        index
        (if uses-flag? "(local $flag i32)\n" "")))
  
(define (emit-wasm-chunk-footer o)
  (fprintf o ")\n"))

(define (code-rel base cur-i)
  (fx- cur-i base))

;; just show decoded instructions from `i` until `start-i`, then
;; generate a chunk function from `start-i` to `end-i`
(define (compile-chunklet o bv i base-i relocs headers labels start-i end-i chunklets fallthrough?)
  (define (in-chunk? target)
    (ormap (lambda (c)
             (and (fx>= target (chunklet-start-i c))
                  (fx< target (chunklet-end-i c))))
           chunklets))
  
  (define (emit-return generated next-instr)
    (append generated 
      `((return (i32.add (local.get $ip) (i32.const ,(code-rel base-i next-instr)))))))
  
  (let loop ([i i] [relocs relocs] [headers headers] [labels labels] [generated (list)])
    (printf "i: ~a\n" i)
    (printf "end-i: ~a\n" end-i)
    
    (cond
      [(and (pair? headers)
            (fx= i (caar headers)))
          (printf "at header: ~a\n" i)
        (let ([size (cdar headers)])
          (cond
            [(fx>= i start-i)
              (if (fx= i end-i) 
                  ;; if we are at the start of a header AND at the end of a chunk,
                  ;; finish off the chunk by returning the address after the header
                  
                  (emit-return generated (fx+ i size))
                  ($oops 'emit-chunk "should have ended at header ~a/~a" i end-i))]
            [else
                (fprintf o ";; data: ~a bytes\n" size)
                (let ([next-i (fx+ i size)])
                  (loop next-i
                        (advance-relocs relocs next-i)
                        (cdr headers)
                        labels
                        generated))]))]
      [(fx= i end-i)
        (printf "i:~a HIT CHUNK END\n" i)
        (emit-return generated i)]
      [(and (pair? labels)
            (fx= i (label-to (car labels))))
       (when (fx>= i start-i)
         (let ([a (car labels)])
           (when (ormap in-chunk? (label-all-from a))
             (fprintf o "label_~x:\n" i))))
       (loop i relocs headers (cdr labels) generated)]
      [else
       (let ([instr (bytevector-s32-ref bv i (constant fasl-endianness))]
             [uinstr (bytevector-u32-ref bv i (constant fasl-endianness))])
       
         (define (unimplemented instr) 
          ($oops 'emit-chunk "instruction opcode ~a is unimplemented" instr))

         (define (next generated)
           (loop (fx+ i instr-bytes) relocs headers labels generated))

         (define (done generated)
           (next generated))

        (define (dr-form _op emit)
          (next (append generated
            `((comment . ,(format ";;~a: r~a <- r~a" 
                        i
                        (instr-dr-dest instr)
                        (instr-dr-reg instr))))
            (emit))))
         
         (define (di-form _op emit)
          (next (append generated
           `((comment . ,(format ";;~a: r~a <- 0x~x"
                    i
                    (instr-di-dest instr)
                    (instr-di-imm instr))))
            (emit))))
         
         (define (drr-form _op emit)
           (next 
            (append generated
              `((comment . ,(format ";;~a: r~a <- r~a, r~a"
                        i
                        (instr-drr-dest instr)
                        (instr-drr-reg1 instr)
                        (instr-drr-reg2 instr))))
              (emit))))
         
         (define (dri-form _op emit)
           (next (append generated 
            `((comment . ,(format ";;~a: r~a <- r~a, 0x~x\n"
                    i
                    (instr-dri-dest instr)
                    (instr-dri-reg instr)
                    (instr-dri-imm instr))))
            (emit))))
        
          (define (r/b-form _op emit)
              (next (append generated
                `((comment . ,(format ";;~a: b r~a" i (instr-dr-reg instr)))) 
                 (emit)
              )))
          
          (define (i/b-form _op emit)
              (next (append generated
                `((comment . ,(format ";;~a b ~a" i (instr-i-imm instr)))) 
                 (emit))))

          (define (di/b-form _op emit)
            (next (append 
                    generated
                    `((comment . ,(format ";;~a: b r~a, ~a" i (instr-di-dest instr) (instr-di-imm instr))))            
                    (emit))))

        (define (emit-skipped)
          (next (append 
                  generated
                  `((comment . ,(format ";; instruction ~a not included" i))))))
        
         (define-syntax (emit stx)
            (syntax-case stx (di/u
                               di di/f dr dr/f fp-dr/f fp-dr dr/x
                               drr fp-drr dri drr/f dri/f dri/c
                               dri/x r d/f d/x i r/b i/b dr/b di/b n n/x
                               adr literal nop ld st rev
                               mov16/z mov16/k mov i->i d->d i->d d->i s->d d->s d->s->d
                               i-bits->d-bits d-bits->i-bits i-i-bits->d-bits
                               d-lo-bits->i-bits d-hi-bits->i-bits
                               binop)
              ; mov16
               [(_ op di/u mov16/z shift) 
                  #`(di-form 'op 
                    (lambda ()
                      (emit-pb-mov16-pb-zero-bits-pb-shift 
                        (instr-di-dest instr)
                        (instr-di-imm instr)
                        #,(case (datum shift)
                          [(shift0) 0]
                          [(shift1) 16]
                          [(shift2) 32]
                          [(shift3) 48])
                        '$ms)))]
               [(_ op di/u mov16/k shift) 
                  #`(di-form 'op
                      (lambda ()
                        (emit-pb-mov16-pb-keep-bits-pb-shift 
                          (instr-di-dest instr)
                          (instr-di-imm instr)
                          #,(case (datum shift)
                            [(shift0) 0]
                            [(shift1) 16]
                            [(shift2) 32]
                            [(shift3) 48])
                          '$ms)))]

              ; mov
               [(_ op dr mov i->i) 
                #'(di-form 'op
                    (lambda ()
                      (emit-pb-mov-pb-i-i 
                        (instr-dr-dest instr) 
                        (instr-dr-reg instr)
                        '$ms)))]

               [(_ op dr mov d->d)
                  #'(dr-form 'op
                    (lambda ()
                      (emit-pb-mov-pb-d-d 
                        (instr-dr-dest instr)
                        (instr-dr-reg instr)
                        '$ms)))]

               [(_ op dr mov i->d)
                  #'(dr-form 'op
                    (lambda ()
                      (emit-pb-mov-pb-i-d
                        (instr-dr-dest instr)
                        (instr-dr-reg instr)
                          '$ms)))]

               [(_ op dr mov d->i) #'(unimplemented 'op)]
               [(_ op dr mov s->d) #'(unimplemented 'op)]
               [(_ op dr mov d->s) #'(unimplemented 'op)]
               [(_ op dr mov d->s->d) #'(unimplemented 'op)]
               [(_ op dr mov i-bits->d-bits) #'(unimplemented 'op)]
               [(_ op dr mov d-bits->i-bits) #'(unimplemented 'op)]
               [(_ op drr mov i-i-bits->d-bits) #'(unimplemented 'op)]
               [(_ op dr mov d-lo-bits->i-bits) #'(unimplemented 'op)]
               [(_ op dr mov d-hi-bits->i-bits) #'(unimplemented 'op)]

               ; cmp
               [(_ op dr/f cmp-op) #'(dr-form 'op 
                                    (lambda () 
                                      (emit-pb-cmp-op-pb-register 
                                        (instr-dr-dest instr)
                                        (instr-dr-reg instr)
                                        '$ms
                                        'cmp-op
                                        '$flag)))]
                
                [(_ op fp-dr/f fp-cmp-op) 
                  #'(dr-form 'op 
                        (lambda () 
                          (emit-pb-fp-cmp-op
                            (instr-dr-dest instr)
                            (instr-dr-reg instr)
                            '$ms
                            '$flag
                            'fp-cmp-op)))]

                [(_ op di/f cmp-op) #'(di-form 'op
                                        (lambda ()
                                          (emit-pb-cmp-op-pb-immediate
                                            (instr-di-dest instr)
                                            (instr-di-imm instr)
                                            '$ms
                                            'cmp-op
                                            '$flag)))]
                
                ; bin ops
               [(_ op drr binop b-op) 
                  #'(drr-form 'op
                      (lambda ()
                        (emit-pb-bin-op-pb-no-signal-pb-register
                          (instr-drr-dest instr)
                          (instr-drr-reg1 instr)
                          (instr-drr-reg2 instr)
                          '$ms
                          'b-op)))]

               [(_ op dri binop b-op) 
                  #'(drr-form 'op 
                      (lambda ()
                        (emit-pb-bin-op-pb-no-signal-pb-immediate 
                          (instr-dri-dest instr)
                          (instr-dri-reg instr)
                          (instr-dri-imm instr)
                          '$ms
                          'b-op)))]

               ; signaling bin ops
               [(_ op drr/f binop b-op) #'(drr-form 'op
                                            (lambda ()
                                              (emit-pb-binop-signal-pb-register
                                                (instr-drr-dest instr)
                                                (instr-drr-reg1 instr)
                                                (instr-drr-reg2 instr)
                                                '$ms
                                                '$flag
                                                'b-op)))]

               [(_ op dri/f binop b-op) #'(dri-form 'op
                                            (lambda ()
                                              (emit-pb-binop-signal-pb-immediate 
                                                (instr-dri-dest instr)
                                                (instr-dri-reg instr)
                                                (instr-dri-imm instr)
                                                '$ms
                                                '$flag
                                                'b-op)))]
              ; fp binops
              [(_ op fp-drr fp-b-op) #'(drr-form 'op 
                                        (lambda () 
                                          (emit-pb-fp-binop
                                            (instr-drr-dest instr)
                                            (instr-drr-reg1 instr)
                                            (instr-drr-reg2 instr)
                                            '$ms
                                            'fp-b-op)))]
              
               [(_ op fp-dr fp-un-op) #'(dr-form 'op 
                                         (lambda ()
                                          (emit-pb-fp-unop
                                            (instr-dr-dest instr)
                                            (instr-dr-reg instr)
                                            '$ms
                                            'fp-un-op)))]
              
               ; ld
               [(_ op dri ld src-type) 
                #'(dri-form 'op
                    (lambda ()
                      (emit-pb-ld-pb-immediate
                        (instr-dri-dest instr) 
                        (instr-dri-reg instr)
                        (instr-dri-imm instr)
                        '$ms
                        'src-type
                        8)))]
                
                [(_ op drr ld src-type)
                  #'(drr-form 'op
                      (lambda ()
                        (emit-pb-ld-pb-register
                          (instr-drr-dest instr)
                          (instr-drr-reg1 instr)
                          (instr-drr-reg2 instr)
                          '$ms
                          'src-type
                          8)))]

                ; st
                [(_ op dri st src-type) 
                  #'(dri-form 'op 
                    (lambda ()
                      (emit-pb-st-pb-immediate 
                        (instr-dri-dest instr)
                        (instr-dri-reg instr)
                        (instr-dri-imm instr)
                        '$ms
                        'src-type
                        8)))]

                [(_ op drr st src-type) 
                  #'(drr-form 'op 
                    (lambda ()
                      (emit-pb-st-pb-register 
                        (instr-drr-dest instr)
                        (instr-drr-reg1 instr)
                        (instr-drr-reg2 instr)
                        '$ms
                        'src-type
                        8)))]

                ; rev
                [(_ op dr rev type) #'(unimplemented 'op)]
                        
                ; b register
                [(_ op r/b test)
                  #'(r/b-form 'op (lambda ()
                          (emit-pb-b-pb-register 
                            (instr-dr-reg instr)
                            (code-rel base-i (fx+ i instr-bytes))
                            '$ms
                            test)))]

               [(_ op i/b test) 
                  #'(i/b-form 'op (lambda ()
                      (let* ([delta (instr-i-imm instr)]
                            [next-instr (fx+ i instr-bytes)]
                            [target-label (fx+ next-instr delta)]) 
                          (emit-pb-b-pb-immediate 
                            (code-rel base-i target-label)
                            (code-rel base-i next-instr)
                            '$ms
                            test))))]
                  
               [(_ op dr/b) #`(unimplemented 'op)]
               [(_ op di/b) #'(unimplemented 'op)
                #'(di/b-form 'op
                  (lambda ()
                    (emit-pb-b*-pb-immediate
                      (instr-di-dest instr)
                      (instr-di-imm instr)
                      '$ms)))]
               [(_ op n) #`(unimplemented 'op)]
               [(_ op n/x) #`(unimplemented 'op)]
               [(_ op adr) #'(next (append generated 
                             `((comment . ,(format ";; adr r~a" (instr-adr-dest instr))))
                              (emit-pb-adr 
                                (instr-adr-dest instr) 
                                '$ms 
                                (instr-adr-imm instr) 
                                (code-rel base-i (fx+ i instr-bytes)))))]

               [(_ op literal)
                #'(let ([dest (instr-di-dest instr)])
                    (unless (and (pair? relocs)
                                 (fx= (fx+ i instr-bytes) (car relocs)))
                      ($oops 'pbchunk "no relocation after pb-literal?"))
                      (let ([gen (append generated 
                                  `((comment . ,(format ";; literal r~a" (instr-di-dest instr))))
                                  (emit-pb-literal 
                                        dest '$ms 
                                        (constant ptr-bytes)
                                        (code-rel base-i (fx+ i instr-bytes))))])
                        (loop (fx+ i instr-bytes (constant ptr-bytes)) (cdr relocs) headers labels gen)))]
               
               [(_ op nop) #'(next generated)]
               [(_ op props ...) #`(unimplemented 'op)]))
              
              (if (< i start-i)
                (emit-skipped)
                (instruction-cases instr emit)))])))

(define (extract-name name)
  (fasl-case* name
    [(string ty string) (list->string
                         (let loop ([l (string->list string)])
                           (cond
                             [(null? l) '()]
                             [(and (eqv? #\* (car l))
                                   (pair? (cdr l))
                                   (eqv? #\/ (cadr l)))
                              ;; mangle to avoid "*/" in name
                              (cons* (car l) #\space (loop (cdr l)))]
                             [else (cons (car l) (loop (cdr l)))])))]
    [(indirect g i) (extract-name (vector-ref g i))]
    [else "???"]))

    (set-who! $fasl-wasm-pbchunk! fasl-wasm-pbchunk!)
)
