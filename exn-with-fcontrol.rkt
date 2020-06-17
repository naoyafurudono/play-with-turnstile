#lang turnstile/quicklang
(require racket/control)

(provide (all-defined-out) quote)

(define-base-types Int Bool)
(define-type-constructor → #:arity > 1)

;; Exn = {'exn:...}

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
     (syntax-parser  ;; syntax-parserはsyntax-parseと違って、stxを引数に取らない。ということで、procを返す。ここでは(_ loc:id ...) -> #'(~and tmp .....)
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

(define-typed-syntax (raise (~datum :) τ s) ≫
  ------------
  [⊢ (#%app- fcontrol s)
     (⇒  τ)
     (⇒ exn (s))])

(define-typed-syntax (try-handle e s handler) ≫
  [⊢ e ≫ e-
     (⇒ : τ-e)
     (⇒ exn (~locs e-exn ...))]
  [⊢ handler ≫ handler-
     (⇐ : τ-e)
     (⇒ exn (~locs handler-exn ...))]
  #:with uncaught-exn
  (remove-s* #'s  (syntax->list #'(e-exn ...  handler-exn ...)));; same-exn?)
  --------------------
  [⊢ (% e- (λ- (x- k-) (if- (#%app- eqv?- s x-)
                            handler-
                            (raise : τ-e x-))))
     (⇒ : τ-e)
     (⇒ exn uncaught-exn)])

  (begin-for-syntax
    (define (remove-s* s lst)
      (if (null? lst)
          '()
          (let ([head (car lst)])
            (if (same-exn? head s)
                (remove-s* s (cdr lst))
                (cons head (remove-s* s (cdr lst)))))))
    (define (quoted-e a)
      (syntax-e (cadr (syntax-e a))))
    (define (same-exn? a b)
      (eqv? (quoted-e a) (quoted-e b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-typed-syntax (if exp1 exp2 exp3) ≫
  [⊢ exp1 ≫ exp1-
     (⇐ Bool)
     (⇒ exn (~locs exn1 ...))]
  [⊢ exp2 ≫ exp2-
     (⇒ τ2)
     (⇒ exn (~locs exn2 ...))]
  [⊢ exp3 ≫ exp3-
     (⇐ τ2)
     (⇒ exn (~locs exn3 ...))]
  --------------------
  [⊢ (if- exp1- exp2- exp3-)
     (⇒ τ2)
     (⇒ exn (exn1 ... exn2 ... exn3 ...))])
;;;;;;;;;;;;;;;;;;;;;;;;; STLC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(try-handle
 (raise : Int 'exn)
 'exn
 12)
