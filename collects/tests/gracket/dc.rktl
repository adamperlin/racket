
(load-relative "loadtest.rktl")

(require racket/gui/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               DC Tests                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mdc (make-object bitmap-dc%))
(define bm (make-object bitmap% 10 10))
(define bm2 (make-object bitmap% 10 10))

(test #t 'is-color? (send bm is-color?))

(define (bad m . args)
  (with-handlers ([exn:fail:contract?
		   (lambda (x)
		     (test '("ok")
			   `(send <bad-dc> ,m ...)
			   (regexp-match "ok" (exn-message x))))])
    (send-generic mdc (make-generic (object-interface mdc) m) . args)
    (error 'bad-dc "~a shouldn't succeed" `(send <bad-dc> ,m ...))))

(define (good m . args)
  (send-generic mdc (make-generic (object-interface mdc) m) . args))

(define (test-all mdc try try-ok)
  (try 'erase)
  (try 'clear)
  (try 'draw-arc 0 0 10 10 0.1 0.2)
  (try 'draw-bitmap bm2 0 0)
  (try 'draw-bitmap-section bm2 0 0 0 0 5 5)
  (try 'draw-ellipse 0 0 10 10)
  (try 'draw-line 0 0 10 10)
  (try 'draw-lines (list (make-object point% 0 0) (make-object point% 10 10)))
  (try 'draw-point 5 5)
  (try 'draw-polygon (list (make-object point% 0 0) (make-object point% 10 10) (make-object point% 5 10)))
  (try 'draw-rectangle 0 0 10 10)
  (try 'draw-rounded-rectangle 0 0 10 10)
  (try 'draw-spline 0 0 10 10 5 10)
  (try 'draw-text "Hello" 0 0)

  (try 'start-doc "Ok")
  (try 'start-page)
  (try 'end-page)
  (try 'end-doc)

  (try 'get-size)

  (try-ok 'get-background)
  (try-ok 'get-brush)
  (try-ok 'get-clipping-region)
  (try-ok 'get-font)
  (try-ok 'get-pen)
  (try-ok 'get-text-background)
  (try-ok 'get-text-foreground)
  (try-ok 'get-text-mode)
  (try-ok 'get-alpha)
  (try-ok 'get-scale)
  (try-ok 'get-origin)
  (try-ok 'get-rotation)

  (try-ok 'set-background (make-object color% "Yellow"))
  (try-ok 'set-brush (make-object brush% "Yellow" 'solid))
  (try-ok 'set-clipping-rect 0 0 10 10)
  (try-ok 'set-clipping-region (make-object region% mdc))
  (try-ok 'set-font (make-object font% 12 'default 'normal 'normal))
  (try-ok 'set-origin 0 0)
  (try-ok 'set-pen (make-object pen% "Yellow" 1 'solid))
  (try-ok 'set-scale 2 2)
  (try-ok 'set-alpha 0.75)
  (try-ok 'set-text-background (make-object color% "Yellow"))
  (try-ok 'set-text-foreground (make-object color% "Yellow"))
  (try-ok 'set-text-mode 'transparent)

  (try-ok 'get-char-height)
  (try-ok 'get-char-width)

  (try 'try-color (make-object color% "Yellow") (make-object color%)))

(st #f mdc ok?)
(test-all mdc bad good)

(send mdc set-bitmap bm)

(test-all mdc 
          (lambda (m . args)
            (send-generic mdc (make-generic (object-interface mdc) m) . args))
          (lambda (m . args)
            (send-generic mdc (make-generic (object-interface mdc) m) . args)))

(send mdc set-bitmap #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get-pixel, set-pixel, get-argb-pixels, etc.

(require mzlib/etc)

(define (pixel-tests b&w?)
  (begin-with-definitions

   (define bm3 (make-object bitmap% 10 10 b&w?))

   (define mdc (make-object bitmap-dc% bm3))
   (send mdc clear)

   (define col (make-object color%))
   (define bts (make-bytes 40))

   (st #f mdc get-pixel 30 4 col)
   (st #t mdc get-pixel 3 4 col)
   (st 255 col red)
   (st 255 col green)
   (st 255 col blue)
   (stv mdc get-argb-pixels 0 0 2 5 bts)
   (test #t 'same-str (equal? (make-bytes 40 255) bts))

   (send col set 30 40 50)
   (send mdc try-color col col)
   (send mdc set-pixel 3 4 col)
   (when b&w?
     (st 0 col red)
     (st 0 col green)
     (st 0 col blue))

   (stv mdc get-argb-pixels 2 1 2 5 bts)
   (test #t 'same-str (equal? (bytes-append (make-bytes 28 255)
					    (bytes 255
						   (send col red)
						   (send col green)
						   (send col blue))
					    (make-bytes 8 255))
			      bts))

   (define col2 (make-object color% 130 140 150))
   (send mdc try-color col2 col2)
   (let loop ([i 0])
     (unless (= i 10)
       (bytes-set! bts (+ 0 (* i 4)) 255)
       (bytes-set! bts (+ 1 (* i 4)) (send col2 red))
       (bytes-set! bts (+ 2 (* i 4)) (send col2 green))
       (bytes-set! bts (+ 3 (* i 4)) (send col2 blue))
       (loop (add1 i))))
   (stv mdc set-argb-pixels 5 5 5 2 bts)
   (let ([col3 (make-object color%)]
	 [white (make-object color% 255 255 255)]
	 [check-col (lambda (what a b)
		      (test #t `(same red ,what, (send a red) ,(send b red)) (= (send a red) (send b red)))
		      (test #t `(same green ,what) (= (send a green) (send b green)))
		      (test #t `(same blue ,what) (= (send a blue) (send b blue))))])
     (let i-loop ([i 0])
       (unless (= i 10)
	 (let j-loop ([j 0])
	   (if (= j 10)
	       (i-loop (add1 i))
	       (begin
		 (st #t mdc get-pixel i j col3)
		 (cond
		  [(and (= i 3) (= j 4))
		   (check-col '(3 4) col col3)]
		  [(and (<= 5 i 9)
			(<= 5 j 6))
		   (check-col `(,i ,j) col2 col3)]
		  [else
		   (check-col `(,i ,j) white col3)])
		 (j-loop (add1 j))))))))))

(pixel-tests #f)
(pixel-tests #t)

;; ----------------------------------------

;; Extra get-argb-pixels on monochrome text (from PR 8821):

(let* ((bm (make-object bitmap% 5 5 #t))
       (bm2 (make-object bitmap% 5 5 #t))
       (dc (new bitmap-dc% (bitmap bm)))
       (pt (lambda (x y) (make-object point% x y)))
       (bs (make-bytes 100))
       (bs2 (make-bytes 100)))
  (send dc clear)
  (send dc set-brush (make-object color% 0 0 0) 'solid)
  (send dc draw-polygon (list (pt 2 0) (pt 2 4)
                              (pt 4 4) (pt 4 0)))
  (send dc set-bitmap #f)
  (send bm get-argb-pixels 0 0 5 5 bs)
  (send dc set-bitmap bm2)
  (send dc set-argb-pixels 0 0 5 5 bs)
  (send dc get-argb-pixels 0 0 5 5 bs2)
  (send dc set-bitmap #f)
  (test #t 'mono-bits
        (equal?
         bs
         (bytes-append 
          #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0\377\0\0\0\377\377\377\377\377"
          #"\377\377\377\377\0\0\0\377\0\0\0\377\0\0\0\377\377\377\377\377\377\377\377\377\0\0\0"
          #"\377\0\0\0\377\0\0\0\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0\377\0\0\0"
          #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0\377\0\0\0")))
  (test #t 'same-bits (equal? bs bs2)))

;; ----------------------------------------
;; Test draw-bitmap-section-smooth

(let* ([bm (make-bitmap 100 100)]
       [dc (make-object bitmap-dc% bm)]
       [bm2 (make-bitmap 70 70)]
       [dc2 (make-object bitmap-dc% bm2)]
       [bm3 (make-bitmap 70 70)]
       [dc3 (make-object bitmap-dc% bm3)])
  (send dc draw-ellipse 0 0 100 100)
  (send dc2 draw-bitmap-section-smooth bm 
        10 10 50 50
        0 0 100 100)
  (send dc3 scale 0.5 0.5)
  (send dc3 set-smoothing 'aligned)
  (send dc3 draw-bitmap bm 20 20)
  (let ([s2 (make-bytes (* 4 70 70))]
        [s3 (make-bytes (* 4 70 70))])
    (send bm2 get-argb-pixels 0 0 70 70 s2)
    (send bm3 get-argb-pixels 0 0 70 70 s3)
    (test #t 'same-scaled (equal? s2 s3))))

;; ----------------------------------------
;; Test some masking combinations

(let ()
  (define u (make-object bitmap% 2 2))
  (define mu (make-object bitmap% 2 2))
  (send u set-argb-pixels 0 0 2 2
        (bytes 255 100 0 0
               255 0 0 0
               255 100 0 0
               255 255 255 255))
  (send mu set-argb-pixels 0 0 2 2
        (bytes 255 0 0 0
               255 255 255 255
               255 0 0 0
               255 255 255 255))
  (send u set-loaded-mask mu)
  (define (try-draw nonce-color mode expect 
                    #:bottom? [bottom? #f])
    (let* ((b&w? (not (eq? mode 'color)))
           (bm (make-object bitmap% 2 2 b&w?))
           (dc (make-object bitmap-dc% bm)))
      (send dc clear)
      (when (eq? mode 'black)
        (send dc set-brush "black" 'solid)
        (send dc draw-rectangle 0 0 2 2))
      ;; Check that draw-bitmap-section really uses the
      ;; section, even in combination with a mask.
      (send dc draw-bitmap-section u 
            0 (if bottom? 1 0)
            0 (if bottom? 1 0) 2 1
            'solid nonce-color (send u get-loaded-mask))
      (send dc set-bitmap #f)
      (let ([s (make-bytes (* 2 2 4))])
        (send bm get-argb-pixels 0 0 2 2 s)
        (when b&w? (send bm get-argb-pixels 0 0 2 2 s #t))
        (test expect 'masked-draw s))))
  (define usual-expect (bytes 255 100 0 0
                              255 255 255 255
                              255 255 255 255
                              255 255 255 255))
  (try-draw (make-object color% "green") 'color usual-expect)
  (try-draw (make-object color%) 'color usual-expect)
  (try-draw (make-object color%) 'white
            ;; For b&w destination, check that the
            ;;  alpha is consistent with the drawn pixels
            (bytes 255 0 0 0
                   0 255 255 255
                   0 255 255 255
                   0 255 255 255))
  (send mu set-argb-pixels 0 0 2 2
        (bytes 255 255 255 255
               255 255 255 255
               255 0 0 0
               255 0 0 0))
  (try-draw (make-object color%) 'black
            #:bottom? #t
            ;; Another b&w destination test, this time
            ;; with a mask that forces black pixels to
            ;; white:
            (bytes 255 0 0 0
                   255 0 0 0
                   255 0 0 0
                   0 255 255 255))
  (send mu set-argb-pixels 0 0 2 2
        (bytes 255 255 255 255
               255 0 0 0
               255 255 255 255
               255 0 0 0))
  (try-draw (make-object color%) 'color
            (bytes 255 255 255 255
                   255 0 0 0
                   255 255 255 255
                   255 255 255 255))
  (let ([dc (make-object bitmap-dc% mu)])
    (send dc erase)
    (send dc set-pen "white" 1 'transparent)
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle 0 0 1 1)
    (send dc set-bitmap #f))
  (try-draw (make-object color%) 'color usual-expect))

;; ----------------------------------------
;; 0 alpha should make the RGB components irrelevant

(let ()
  (define bm1 (make-bitmap 1 2))
  (define bm2 (make-bitmap 1 2))

  (send bm1 set-argb-pixels 0 0 1 2 (bytes 0   0 0 0
                                           255 0 0 255))
  (send bm2 set-argb-pixels 0 0 1 2 (bytes 255 255 0 0
                                           0   0   0 0))

  (define the-bytes (make-bytes 8 0))

  (define bm3 (make-bitmap 1 2))
  (define bdc (make-object bitmap-dc% bm3))
  (void (send bdc draw-bitmap bm1 0 0))
  (void (send bdc draw-bitmap bm2 0 0))

  (send bdc get-argb-pixels 0 0 1 2 the-bytes)
  (test (bytes 255 255 0 0
               255 0   0 255)
        values
        the-bytes))

;; ----------------------------------------

;; Check B&W drawing to B&W, 'solid vs. 'opaque
(let ([mk
       (lambda (expect style bg-col col mask?)
         (let* ((bm1 (make-object bitmap% 2 2 #t))
                (bm2 (make-object bitmap% 2 2 #t))
                (bm3 (make-object bitmap% 2 2 #t))
                (dc1 (new bitmap-dc% (bitmap bm1)))
                (dc2 (new bitmap-dc% (bitmap bm2)))
                (dc3 (new bitmap-dc% (bitmap bm3)))
                (s (make-bytes 16)))
           (send dc1 clear)
           (send dc1 set-argb-pixels 0 0 2 1 #"\xFF\0\0\0\xFF\0\0\0")
           (send dc2 clear)
           (send dc2 set-argb-pixels 0 1 2 1 #"\xFF\0\0\0\xFF\0\0\0")
           (send dc3 set-argb-pixels 0 0 2 2 (bytes-append #"\xFF\0\0\0\xFF\xFF\xFF\xFF"
                                                           #"\xFF\0\0\0\xFF\xFF\xFF\xFF"))
           (send dc2 set-background bg-col)
           (send dc2 draw-bitmap bm1 0 0 style col (and mask? bm3))
           (send dc2 set-bitmap #f)
           (send bm2 get-argb-pixels 0 0 2 2 s)
           (let ([col->str (lambda (c)
                             (if (zero? (send c red)) "black" "white"))])
             (test expect `(mk ,style ,(col->str bg-col) ,(col->str col), mask?) s))))]
      [black (make-object color%)]
      [white (make-object color% 255 255 255)])
  (mk #"\377\0\0\0\377\0\0\0\377\0\0\0\377\0\0\0" 'solid white black #f)
  (mk #"\377\0\0\0\377\0\0\0\377\0\0\0\377\0\0\0" 'solid black black #f)
  (mk #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0" 'solid black white #f)
  (mk #"\377\0\0\0\377\377\377\377\377\0\0\0\377\0\0\0" 'solid white black #t)
  (mk #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0" 'solid white white #t)
  (mk #"\377\0\0\0\377\0\0\0\377\377\377\377\377\377\377\377" 'opaque white black #f)
  (mk #"\377\0\0\0\377\0\0\0\377\0\0\0\377\0\0\0" 'opaque black black #f)
  (mk #"\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377" 'opaque white white #f)
  (mk #"\377\0\0\0\377\377\377\377\377\377\377\377\377\0\0\0" 'opaque white black #t)
  (mk #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0" 'opaque black white #t))

;; ----------------------------------------
;; check get-alpha mode of `get-argb-pixels'

(let ()
  (define (get-column-alpha bm x y)
    (define bs (make-bytes 4))
    (send bm get-argb-pixels x y 1 1 bs #t)
    bs)
  (define abm (make-object bitmap% 2 2 #f #t))
  (define nbm (make-object bitmap% 2 2 #f #f))
  (define (avg bstr) (- 255
                        (quotient (+ (bytes-ref bstr 0)
                                     (bytes-ref bstr 1)
                                     (bytes-ref bstr 2))
                                  3)))
  (send abm set-argb-pixels 0 0 2 2 #"0123456789abcdef")
  (send nbm set-argb-pixels 0 0 2 2 #"0123456789abcdef") ; alphas ignored

  (test (bytes (char->integer #\0) 0 0 0) 'a0+0 (get-column-alpha abm 0 0))
  (test (bytes (char->integer #\4) 0 0 0) 'a1+0 (get-column-alpha abm 1 0))
  (test (bytes (char->integer #\8) 0 0 0) 'a0+1 (get-column-alpha abm 0 1))
  (test (bytes (char->integer #\c) 0 0 0) 'a1+1 (get-column-alpha abm 1 1))

  (test (bytes (avg #"123") 0 0 0) 'n0+0 (get-column-alpha nbm 0 0))
  (test (bytes (avg #"567") 0 0 0) 'n1+0 (get-column-alpha nbm 1 0))
  (test (bytes (avg #"9ab") 0 0 0) 'n0+1 (get-column-alpha nbm 0 1))
  (test (bytes (avg #"def") 0 0 0) 'n1+1 (get-column-alpha nbm 1 1)))

;; ----------------------------------------
;; check pre-mult mode of `{get,set}-argb-pixels'

(let ()
  (define abm (make-object bitmap% 2 2 #f #t))
  (define nbm (make-object bitmap% 2 2 #f #f))
  (send abm set-argb-pixels 0 0 2 2 #"30127456b89afcde" #f #t)
  (send nbm set-argb-pixels 0 0 2 2 #"0123456789abcdef" #f #t) ; alphas ignored

  (define (get-pixels bm pre-mult?)
    (define bs (make-bytes 16))
    (send bm get-argb-pixels 0 0 2 2 bs #f pre-mult?)
    bs)

  (define (unmul b)
    (define (um v) (inexact->exact (round (/ (* v 255.) (bytes-ref b 0)))))
    (bytes (bytes-ref b 0)
           (um (bytes-ref b 1))
           (um (bytes-ref b 2))
           (um (bytes-ref b 3))))

  (test #"\xFF123\xFF567\xFF9ab\xFFdef" 'no-alpha (get-pixels nbm #f))
  (test #"\xFF123\xFF567\xFF9ab\xFFdef" 'no-alpha (get-pixels nbm #t))

  (test (apply bytes-append (map unmul '(#"3012" #"7456" #"b89a" #"fcde")))
        'alpha-normal (get-pixels abm #f))
  (test #"30127456b89afcde" 'alpha-premult (get-pixels abm #t)))

;; ----------------------------------------
;; check consistency of pre-multiplication, drawing, etc.

(let ()
  (define gray-cols (make-bitmap 256 256 #f)) ; no alpha channel
  (let ([s (make-bytes (* 256 256 4))])
    (for* ([i 256] [j 256])
      (bytes-set! s (+ (* 4 i) (* j 256 4)) 255)
      (bytes-set! s (+ (* 4 i) 1 (* j 256 4)) (- 255 i))
      (bytes-set! s (+ (* 4 i) 2 (* j 256 4)) (- 255 i))
      (bytes-set! s (+ (* 4 i) 3 (* j 256 4)) (- 255 i)))
    (send gray-cols set-argb-pixels 0 0 256 256 s))

  (define rainbow-rows (make-bitmap 256 256))
  (let ([s (make-bytes (* 256 256 4))])
    (for* ([i 256] [j 256])
      (bytes-set! s (+ (* 4 i) (* j 256 4)) 255)
      (bytes-set! s (+ (* 4 i) 1 (* j 256 4)) j)
      (bytes-set! s (+ (* 4 i) 2 (* j 256 4)) (modulo (+ j 10) 256))
      (bytes-set! s (+ (* 4 i) 3 (* j 256 4)) (modulo (+ j 20) 256)))
    (send rainbow-rows set-argb-pixels 0 0 256 256 s))

  (define rainbow-rows-alpha-cols (make-bitmap 256 256))
  (let ([s (make-bytes (* 256 256 4))])
    (for* ([i 256] [j 256])
      (bytes-set! s (+ (* 4 i) (* j 256 4)) i)
      (bytes-set! s (+ (* 4 i) 1 (* j 256 4)) j)
      (bytes-set! s (+ (* 4 i) 2 (* j 256 4)) (modulo (+ j 10) 256))
      (bytes-set! s (+ (* 4 i) 3 (* j 256 4)) (modulo (+ j 20) 256)))
    (send rainbow-rows-alpha-cols set-argb-pixels 0 0 256 256 s))

  (define rainbow-rows-alpha-cols-premult (make-bitmap 256 256))
  (let ([s (make-bytes (* 256 256 4))])
    (for* ([i 256] [j 256])
      (bytes-set! s (+ (* 4 i) (* j 256 4)) i)
      (bytes-set! s (+ (* 4 i) 1 (* j 256 4)) (min i j))
      (bytes-set! s (+ (* 4 i) 2 (* j 256 4)) (min i (modulo (+ j 10) 256)))
      (bytes-set! s (+ (* 4 i) 3 (* j 256 4)) (min i (modulo (+ j 20) 256))))
    (send rainbow-rows-alpha-cols-premult set-argb-pixels 0 0 256 256 s #f #t))

  ;; Check that drawing with a mask is consistent with `set-argb-pixels'
  ;; in non-premultiplied mode:
  (let ([target (make-bitmap 256 256)])
    (define dc (make-object bitmap-dc% target))
    (send dc draw-bitmap rainbow-rows 0 0
          'solid
          (send the-color-database find-color "black")
          gray-cols)
    (let ([s1 (make-bytes (* 256 256 4))]
          [s2 (make-bytes (* 256 256 4))])
      (send target get-argb-pixels 0 0 256 256 s1 #f #t)
      (send rainbow-rows-alpha-cols get-argb-pixels 0 0 256 256 s2 #f #t)
      (for ([i (in-range (* 256 256))])
        (unless (= (bytes-ref s1 i) (bytes-ref s2 i))
          (printf "~a ~a ~a\n" i (bytes-ref s1 i) (bytes-ref s2 i))))
      (test #t 'consistent-mult (equal? s1 s2))))

  ;; Check that getting non-premult values out and putting them back in
  ;; gives consistent premult results:
  (let ([target (make-bitmap 256 256)])
    (let ([s1 (make-bytes (* 256 256 4))]
          [s2 (make-bytes (* 256 256 4))])
      (send rainbow-rows-alpha-cols-premult get-argb-pixels 0 0 256 256 s1 #f #f)
      (send target set-argb-pixels 0 0 256 256 s1 #f #f)
      
      (send target get-argb-pixels 0 0 256 256 s1 #f #t)
      (send rainbow-rows-alpha-cols-premult get-argb-pixels 0 0 256 256 s2 #f #t)
      (test #t 'consistent-premult (equal? s1 s2))))

  (void))

;; ----------------------------------------

(let ()
  (define bm (make-screen-bitmap 100 100))
  (define dc (make-object bitmap-dc% bm))
  (define-values (aw ah aa ad) (send dc get-text-extent "x " #f #t))
  (define-values (bw bh ba bd) (send dc get-text-extent "x ⇒ y" #f #t))
  (test #t 'no-missing-glyph-truncation (bw . > . aw)))

;; ----------------------------------------

(test #f 'no-commas (ormap (lambda (s) (regexp-match? #rx"," s)) (get-face-list)))
(test #t 'all-commas (andmap (lambda (s) (regexp-match? #rx"," s)) (get-face-list #:all-variants? #t)))

;; ----------------------------------------

(define (check-immutable v)
  (test 'immutable 'immutable
        (with-handlers ([exn:fail? (lambda (x) 
                                     (if (regexp-match #rx"immutable" (exn-message x))
                                         'immutable
                                         x))])
          (send v set-color "red"))))

(check-immutable (send the-brush-list find-or-create-brush "white" 'solid))
(check-immutable (send the-pen-list find-or-create-pen "white" 1 'solid))

;; ----------------------------------------

(let ([color (new color%)])
  (test #f 'color (send color is-immutable?))
  (test 0 'color (send color red))
  (test 0 'color (send color green))
  (test 0 'color (send color blue))
  (test 1.0 'color (send color alpha)))

(let ([color (make-color 101 102 103 0.9)])
  (test #t 'color (send color is-immutable?))
  (test 101 'color (send color red))
  (test 102 'color (send color green))
  (test 103 'color (send color blue))
  (test 0.9 'color (send color alpha)))

(let ([color (make-color 0 0 0)])
  (test #t 'color (send color is-immutable?))
  (test 0 'color (send color red))
  (test 0 'color (send color green))
  (test 0 'color (send color blue))
  (test 1.0 'color (send color alpha)))

;; ----------------------------------------

(let ([brush (new brush%)])
  (test #f 'brush (send brush is-immutable?)))

(let ([brush (make-brush)])
  (test #t 'brush (send brush is-immutable?))
  (test #t 'brush (eq? brush (send the-brush-list find-or-create-brush "black" 'solid))))

(let ([brush (make-brush #:immutable? #f)])
  (test #f 'brush (send brush is-immutable?)))

;; ----------------------------------------

(let ([pen (new pen%)])
  (test #f 'pen (send pen is-immutable?)))

(let ([pen (make-pen)])
  (test #t 'pen (send pen is-immutable?))
  (test #t 'pen (eq? pen (send the-pen-list find-or-create-pen "black" 0 'solid))))

(let ([pen (make-pen #:immutable? #f)])
  (test #f 'pen (send pen is-immutable?)))

;; ----------------------------------------

(let ()
  (define config (new gl-config%))
  (define bm1 (make-gl-bitmap 100 100 config))
  (define bm2 (make-gl-bitmap 100 100 config))
  (define dc1 (make-object bitmap-dc% bm1))
  (define dc2 (make-object bitmap-dc% bm2))
  (define gl1 (send dc1 get-gl-context))
  (define gl2 (send dc2 get-gl-context))
  (when (and gl1 gl2)
    (send gl1 call-as-current
          (lambda ()
            (test 5 'alt (send gl2 call-as-current
                               (lambda () (error "not in this context!"))
                               (wrap-evt always-evt (lambda (v) 5))))
            (sync
             (thread
              (lambda ()
                (test 8 'thread/alts
                      (send gl1 call-as-current
                            (lambda () (error "not in this thread!"))
                            (wrap-evt always-evt (lambda (v) 8)))))))
            (test 8 'reenter (send gl1 call-as-current
                                   (lambda () 8)))))
    (with-handlers ([exn? void])
      (send gl1 call-as-current (lambda () (error "fail"))))
    (test 12 'post-exn (send gl1 call-as-current (lambda () 12)))))

;; ----------------------------------------

(report-errs)
