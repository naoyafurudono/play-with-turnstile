#lang turnstile/quicklang

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

(define-typed-syntax (raise e) ≫
  [⊢ e ≫ e-
     (⇒ τ)
     (⇒ exn (~locs er ...))
     ]
  ------------
  [⊢ (#%app- raise- e- )
     (⇒  τ)
     (⇒ exn (e er ...))])

(define-typed-syntax (with-handlers ([p handler]) e) ≫
  [⊢ e ≫ e-
     (⇒ : τ-e)
     (⇒ exn (~locs e-exn ...))]
  [⊢ p ≫ p-
     (⇐ : (→ τ-e Bool)  ;; 3rd suggestion
        ;; (⇒ (~→ τ-p-in ~Bool) ;; 2nd suggestion
        (⇒ exn (~locs p-exn ...)))]
  ;; [τ-p-in τ= τ-e #:for p]  ;; 2nd suggestion
  [⊢ handler ≫ handler-
     (⇒ : τ-handler
        (⇒ exn (~locs handler-exn ...)))]
  #:with τ-handler-expected ((current-type-eval) #'(→ τ-e τ-e))
  [τ-handler τ= τ-handler-expected #:for handler]
  ;; #:with uncaught-exn (remove (syntax->list #'(e-exn ...)) p)
  --------------------
  [⊢ (with-handlers- ([p- handler-]) e-)
     (⇒ : τ-e)
     (⇒ exn (e-exn ... p-exn ... handler-exn ...))])

;; We assume exception is represented as Int value, and modify the input type of the predicate to Int.
;; (define-typed-syntax (with-handlers ([p handler]) e) ≫
;;   [⊢ e ≫ e-
;;      (⇒ : τ-e
;;         (⇒ exn (~locs e-exn ...)))]
;;   [⊢ p ≫ p-
;;      (⇒ : (~→ ~Int ~Bool)
;;         (⇒ exn (~locs p-exn ...)))]
;;   [⊢ handler ≫ handler-
;;      (⇒ : (~→ ~Int τ-handler-out)
;;         (⇒ exn (~locs handler-exn ...)))]
;;   [τ-handler-out τ= τ-e]
;;   --------------------
;;   [⊢ (with-handlers- ([p- handler-]) e-)
;;      (⇒ : τ-e)
;;      (⇒ exn (e-exn ... p-exn ... handler-exn ...))])
;; ;;;;;;;;;;;;;;;;;;;;;;; STLC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-typed-syntax (λ ([x:id (~datum :) τ:type] ...) e) ≫
  [[x ≫ x- : τ] ... ⊢ e ≫ e-
                (⇒ : τ-out)
                (⇒ exn (~locs body-exn ...))
                ]
  ------------
  [⊢ (λ- (x- ...) e-)
     (⇒ : (→ τ ... τ-out)
        (⇒ exn (body-exn ...))
        )])

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

