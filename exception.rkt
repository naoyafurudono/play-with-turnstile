#lang turnstile/quicklang

;; λ項 + 足し算 + 例外
;; 今できること : with-handlers以外
;; 例えばeffectを集めることはできている. with-handlerの型検査やeffectを取り除くことができていない
(provide (all-defined-out))

(define-base-types Int Bool)
(define-type-constructor → #:arity > 1)

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
   ------------
   [⊢ (#%datum- . n) ⇒ Int]]
  [(_ . b:boolean) ≫
   ------------
   [⊢ (#%datum- . b) ⇒ Bool]]
  [(_ . x) ≫
   ------------
   [#:error (type-error #:src #'x #:msg "Unsupported literal ~v" #'x)]])

;;;;;;;;;;;; copy and paste. TODO understand here ;;;;;;;;;;;;
(begin-for-syntax
  (define-syntax ~locs
    (pattern-expander
     (syntax-parser
       [(_ loc:id ...)
        #:with tmp (generate-temporary 'locs)
        #'(~and tmp
                (~parse (loc ...) (stx-or #'tmp #'())))])))

  (define (stx-truth? a)
    (and a (not (and (syntax? a) (false? (syntax-e a))))))
  (define (stx-or a b)
    (cond [(stx-truth? a) a]
          [else b]))
  )

;;;;;;;;;;;;;;;;;;;; exception ;;;;;;;;;;;;;;;;;;;;

;; (raise e) = (raise- e-)
;; effectはeのeffect + e (e- ではない。handlerに渡すので)
(define-typed-syntax (raise e) ≫
  [⊢ e ≫ e-
     (⇒ τ)
     (⇒ exn (~locs er ...))
     ]
  ------------
  [⊢ (#%app- raise- e- )
     (⇒  τ)
     (⇒ exn (e er ...))])

;; この式のeffect = p, handlerのeffect, (eのeffect - pを満たすもの)
;; effectを取り除く方法に苦戦してる。マクロの知識?
;; とりあえずhandlerは一つ. (最終的には任意個にする)
(define-typed-syntax (with-handlers ([p handler]) e) ≫
  [⊢ p ≫ p-
     (⇒ : (~→ τ-p-in τ-p-out)
        (⇒ exn (~locs p-exn ...)))]
  ;; TODO check τ-p-out = Bool
  ;; #:fail-unless (type=? #'τ-p-out #'Bool) (print "error")  ;; NOT work well
  [⊢ handler ≫ handler-
     (⇒ (~→ τ-h-in τ-h-out))
     (⇒ exn (~locs handler-exn ...))]
  ;; TODO check τ-p-in = τ-h-in
  [⊢ e ≫ e-
      (⇒ : τ-e)
     (⇒ exn (~locs e-exn ...))]
  ;; TODO check τ-h-in = τ-e
  ;; #:with uncaught-exn (remove #t (syntax->list #'(e-exn ...)) p)  ;; TODO handleするeffectを取り除く
  #:fail-unless #f (print #'(p-exn ... handler-exn ... e-exn ...))  ;; 全てのeffectをprint
    --------------------
  [⊢ (with-handlers- ([p- handler-] e-))
     (⇒ : τ-e)
     (⇒ exn (p-exn ...  handler-exn ...
                   ;; uncaught-exn ...
                   ))])


;;;;;;;;;;;;;;;;;;;;;;; STLC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-typed-syntax (λ ([x:id (~datum :) τ:type] ...) e) ≫
  [[x ≫ x- : τ] ... ⊢ e ≫ e-
                (⇒ : τ-out)
                (⇒ exn (~locs body-exn ...))
                ]
  ;; #:fail-unless #f (print #'(body-exn ...))
  ------------
  [⊢ (λ- (x- ...) e-)
     (⇒ : (→ τ ... τ-out)
        (⇒ exn (body-exn ...))
        )])  ;; この式にはeffectはつかない

(define-typed-syntax (#%app e-fn e-arg ...) ≫
  [⊢ e-fn ≫ e-fn-
     (⇒ : (~→ τ-in ... τ-out)
        (⇒ exn (~locs ty-exn ...)))
     (⇒ exn (~locs fn-exn ...))]
  #:fail-unless (stx-length=? #'[τ-in ...] #'[e-arg ...])
                (num-args-fail-msg #'e-fn #'[τ-in ...] #'[e-arg ...])
  [⊢ e-arg ≫ e-arg-
     (⇐ τ-in)
     (⇒ exn (~locs arg-exn ...))] ...
  ------------
  [⊢ (#%app- e-fn- e-arg- ...)
     (⇒ : τ-out)
     (⇒ exn (ty-exn ... fn-exn ... arg-exn ... ...))
     ])

;;;;;;;;;;;;;;;;;;;; arithmetic ;;;;;;;;;;;;
(define-typed-syntax (+ e1 e2) ≫
  [⊢ e1 ≫ e1-
     (⇐ : Int)
     (⇒ exn (~locs ex1 ...))]
  [⊢ e2 ≫ e2-
     (⇐ : Int)
     (⇒ exn (~locs ex2 ...))]
  ------------
  [⊢ (#%app- +- e1- e2-)
     (⇒ : Int)
     (⇒ exn (ex1 ... ex2 ...))]
  )

(define-typed-syntax (< e1 e2) ≫
  [⊢ e1 ≫ e1-
     (⇐ : Int)
     (⇒ exn (~locs ex1 ...))]
  [⊢ e2 ≫ e2-
     (⇐ : Int)
     (⇒ exn (~locs ex2 ...))]
  ------------
  [⊢ (#%app- <- e1- e2-)
     (⇒ : Bool)
     (⇒ exn (ex1 ... ex2 ...))]
  )

